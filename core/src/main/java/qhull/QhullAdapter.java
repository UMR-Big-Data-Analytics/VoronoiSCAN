package qhull;

import org.qhull.qhT;
import org.qhull.qhull_wrapper_h;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.util.*;

public class QhullAdapter {
    public record QhullResult(Map<Integer, Set<Integer>> adjacency) {
    }

    public static QhullResult delaunayTriangulation(float[][] points) {
        if (points == null || points.length == 0 || points[0].length == 0) {
            return new QhullResult(Collections.emptyMap());
        }
        double[][] doublePoints = new double[points.length][points[0].length];
        for (int i = 0; i < points.length; i++) {
            for (int j = 0; j < points[i].length; j++) {
                doublePoints[i][j] = points[i][j];
            }
        }
        return delaunayTriangulation(doublePoints);

    }

    public static QhullResult delaunayTriangulation(double[][] points) {
        if (points == null || points.length == 0 || points[0].length == 0) {
            return new QhullResult(Collections.emptyMap());
        }
        try (Arena arena = Arena.ofConfined()) {
            int dim = points[0].length;
            int numPoints = points.length;
            MemorySegment pointsArray = arena.allocateArray(qhull_wrapper_h.C_DOUBLE, numPoints * dim);

            for (int i = 0; i < numPoints; i++) {
                for (int j = 0; j < dim; j++) {
                    pointsArray.setAtIndex(qhull_wrapper_h.C_DOUBLE, i * dim + j, points[i][j]);
                }
            }

            MemorySegment qh = qhT.allocate(arena);

            String qhullCommand = "qhull d Qbb Qc Qz Fv";
            int ret = qhull_wrapper_h.qh_new_qhull(qh, dim, numPoints, pointsArray, 0,
                    arena.allocateUtf8String(qhullCommand), MemorySegment.NULL, MemorySegment.NULL);

            if (ret != 0) {
                System.err.println("Qhull failed with exit code " + ret);
                return new QhullResult(Collections.emptyMap());
            }

            Map<Integer, Set<Integer>> adjacency = new HashMap<>();

            MemorySegment facet = qhull_wrapper_h.qh_get_facet_list(qh);
            while (facet.address() != qhull_wrapper_h.qh_get_facet_tail(qh).address()) {
                if (!qhull_wrapper_h.qh_facet_isupper(facet)) {
                    MemorySegment vertexSet = qhull_wrapper_h.get_vertices(facet);
                    if (vertexSet.address() != MemorySegment.NULL.address()) {
                        int count = qhull_wrapper_h.qh_setsize(qh, vertexSet);
                        if (count >= dim) {
                            List<Integer> vertexIndices = new ArrayList<>();
                            for (int i = 0; i < count; i++) {
                                MemorySegment vertex = qhull_wrapper_h.get_vertex_from_set(vertexSet, i);
                                int id = qhull_wrapper_h.get_vertex_index(qh, vertex);
                                vertexIndices.add(id);
                            }

                            for (int i = 0; i < vertexIndices.size(); i++) {
                                for (int j = i + 1; j < vertexIndices.size(); j++) {
                                    int a = vertexIndices.get(i);
                                    int b = vertexIndices.get(j);
                                    adjacency.computeIfAbsent(a, k -> new HashSet<>()).add(b);
                                    adjacency.computeIfAbsent(b, k -> new HashSet<>()).add(a);
                                }
                            }
                        }
                    }
                }

                facet = qhull_wrapper_h.get_next_facet(facet);
            }

            return new QhullResult(adjacency);
        }
    }
}

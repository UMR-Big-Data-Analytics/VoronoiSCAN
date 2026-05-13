package kahip;

import org.kahip.kaffpa.kahip_wrapper_h;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class KahipAdapter {

    public record KahipResult(Map<Integer, Integer> partitionMap, int edgecut) {
    }

    public static void main(String[] args) {
        // CSR format for graph
        int[] xadj = {0, 2, 5, 7, 9, 11}; // Starting indices for each vertex's adjacency list
        int[] adjncy = {1, 4, 0, 2, 4, 1, 3, 2, 4, 0, 3}; // Adjacency list
        int[] adjcwgt = new int[adjncy.length]; // Edge weights (all 1 for unweighted)
        Arrays.fill(adjcwgt, 1);
        final var result = kahipPartition(
                5, // Number of vertices
                new int[]{1, 1, 1, 1, 1}, // Vertex weights (all 1 for unweighted)
                xadj,
                adjncy,
                adjcwgt,
                2, // Number of partitions
                0.03, // Maximum allowed imbalance
                false, // suppress_output
                42 // random seed
        );
        System.out.println("Partitioning result: " + result.partitionMap());
        System.out.println("Edgecut: " + result.edgecut());
    }

    public static KahipResult kahipPartition(
            int numVertices,
            int[] vwgt,
            int[] xadj,
            int[] adjncy,
            int[] adjcwgt,
            int nparts,
            double imbalance,
            boolean suppressOutput,
            int randomSeed) {
        try (Arena arena = Arena.ofConfined()) {
            // Create memory segments for input parameters
            MemorySegment nSegment = arena.allocate(ValueLayout.JAVA_INT);
            nSegment.set(ValueLayout.JAVA_INT, 0, numVertices);

            MemorySegment npartsSegment = arena.allocate(ValueLayout.JAVA_INT);
            npartsSegment.set(ValueLayout.JAVA_INT, 0, nparts);

            MemorySegment imbalanceSegment = arena.allocate(ValueLayout.JAVA_DOUBLE);
            imbalanceSegment.set(ValueLayout.JAVA_DOUBLE, 0, imbalance);

            MemorySegment vwgtSegment = arena.allocateArray(ValueLayout.JAVA_INT, vwgt);
            MemorySegment xadjSegment = arena.allocateArray(ValueLayout.JAVA_INT, xadj);
            MemorySegment adjncySegment = arena.allocateArray(ValueLayout.JAVA_INT, adjncy);
            MemorySegment adjcwgtSegment = arena.allocateArray(ValueLayout.JAVA_INT, adjcwgt);

            // Output parameters
            MemorySegment edgecutSegment = arena.allocate(ValueLayout.JAVA_INT);
            MemorySegment partSegment = arena.allocateArray(ValueLayout.JAVA_INT, numVertices);

            // Call kaffpa
            kahip_wrapper_h.kaffpa(
                    nSegment,
                    vwgtSegment,
                    xadjSegment,
                    adjcwgtSegment,
                    adjncySegment,
                    npartsSegment,
                    imbalanceSegment,
                    suppressOutput,
                    randomSeed,
                    kahip_wrapper_h.ECOSOCIAL(),
                    edgecutSegment,
                    partSegment
            );

            // Read results
            int edgecut = edgecutSegment.get(ValueLayout.JAVA_INT, 0);

            Map<Integer, Integer> partitionMap = new HashMap<>();
            for (int i = 0; i < numVertices; i++) {
                int partition = partSegment.getAtIndex(ValueLayout.JAVA_INT, i);
                partitionMap.put(i, partition);
            }

            return new KahipResult(partitionMap, edgecut);
        }
    }
}
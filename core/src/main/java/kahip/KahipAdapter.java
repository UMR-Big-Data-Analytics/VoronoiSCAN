package kahip;

import org.kahip.kaffpa.kahip_wrapper_h;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.util.HashMap;
import java.util.Map;

public class KahipAdapter {

    public record KahipResult(Map<Integer, Integer> partitionMap, int edgecut) {
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

            MemorySegment edgecutSegment = arena.allocate(ValueLayout.JAVA_INT);
            MemorySegment partSegment = arena.allocateArray(ValueLayout.JAVA_INT, numVertices);

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

#ifndef QHULL_WRAPPER_H
#define QHULL_WRAPPER_H

#include "libqhull_r/qhull_ra.h"
#include <stddef.h>  // for ptrdiff_t
#include <stdbool.h> // for bool return values

#ifdef __cplusplus
extern "C" {
#endif

// Helper to get i-th vertex from facet's vertex set
vertexT* get_vertex_from_set(setT* set, int index);

// Convert vertex->point to index based on original point array
int get_vertex_index(qhT* qh, vertexT* vertex);

// Linked list traversal
facetT* qh_get_facet_list(const qhT* qh);
facetT* qh_get_facet_tail(const qhT* qh);
facetT* get_next_facet(facetT* facet);

// Vertex access
setT* get_vertices(facetT* facet);

// Check for upper Delaunay facet
bool qh_facet_isupper(facetT* facet);

bool qh_facet_is_good(facetT* facet);

#ifdef __cplusplus
}
#endif

#endif // QHULL_WRAPPER_H

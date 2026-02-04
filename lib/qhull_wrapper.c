#include "qhull_wrapper.h"

vertexT* get_vertex_from_set(setT* set, int index) {
    return (vertexT*)SETelem_(set, index);
}

int get_vertex_index(qhT* qh, vertexT* vertex) {
    return qh_pointid(qh, vertex->point);
}

facetT* qh_get_facet_list(const qhT* qh) {
    return qh->facet_list;
}

facetT* qh_get_facet_tail(const qhT* qh) {
    return qh->facet_tail;
}

facetT* get_next_facet(facetT* facet) {
    return facet->next;
}

setT* get_vertices(facetT* facet) {
    return facet->vertices;
}

bool qh_facet_isupper(facetT* facet) {
    return facet->upperdelaunay;
}

bool qh_facet_is_good(facetT* facet) {
    return facet->good;
}

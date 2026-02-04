#include <stdbool.h>

// Constants from original header
#define FAST           0
#define ECO            1
#define STRONG         2
#define FASTSOCIAL     3
#define ECOSOCIAL      4
#define STRONGSOCIAL   5

#define MAPMODE_MULTISECTION 0
#define MAPMODE_BISECTION 1

// Function declarations without default arguments
void kaffpa(int* n, int* vwgt, int* xadj,
           int* adjcwgt, int* adjncy, int* nparts,
           double* imbalance, bool suppress_output, int seed, int mode,
           int* edgecut, int* part);

void kaffpa_balance(int* n, int* vwgt, int* xadj,
                   int* adjcwgt, int* adjncy, int* nparts,
                   double* imbalance,
                   bool perfectly_balance,
                   bool suppress_output, int seed, int mode,
                   int* edgecut, int* part);

void kaffpa_balance_NE(int* n, int* vwgt, int* xadj,
                int* adjcwgt, int* adjncy, int* nparts,
                double* imbalance, bool suppress_output, int seed, int mode,
                int* edgecut, int* part);

void process_mapping(int* n, int* vwgt, int* xadj,
                   int* adjcwgt, int* adjncy,
                   int* hierarchy_parameter, int* distance_parameter, int hierarchy_depth,
                   int mode_partitioning, int mode_mapping,
                   double* imbalance,
                   bool suppress_output, int seed,
                   int* edgecut, int* qap, int* part);

void node_separator(int* n, int* vwgt, int* xadj,
                    int* adjcwgt, int* adjncy, int* nparts,
                    double* imbalance, bool suppress_output, int seed, int mode,
                    int* num_separator_vertices, int** separator);

void reduced_nd(int* n, int* xadj, int* adjncy,
                bool suppress_output, int seed, int mode,
                int* ordering);

// Modified function without default argument
void edge_partitioning(int* n, int* vwgt, int* xadj,
                   int* adjcwgt, int* adjncy, int* nparts,
                   double* imbalance, bool suppress_output, int seed, int mode,
                   int* vertexcut, int* part, int infinity_edge_weight);

#ifdef USEMETIS
void reduced_nd_fast(int* n, int* xadj, int* adjncy,
                     bool suppress_output, int seed, int* ordering);
#endif
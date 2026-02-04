from densired import datagen
import pandas as pd


def generate_data(dimension, num=10_000_000):
    x = datagen.densityDataGen(dim=dimension, ratio_noise=0.1, max_retry=5,
                               dens_factors=[1, 1, 0.5, 0.3, 2, 1.2, 0.9, 0.6, 1.4, 1.1], square=True,
                               clunum=10, seed=6, core_num=200,
                               momentum=[0.5, 0.75, 0.8, 0.3, 0.5, 0.4, 0.2, 0.6, 0.45, 0.7],
                               branch=[0, 0.05, 0.1, 0, 0, 0.1, 0.02, 0, 0, 0.25],
                               con_min_dist=0.8, verbose=True, safety=True, domain_size=20, random_start=False)
    data = x.generate_data(num)
    return data[:, 0:-1]


def write_data_to_csv_file(data, filename):
    df = pd.DataFrame(data)
    df.to_csv(filename + ".csv", index=False, header=False)


if __name__ == "__main__":
    dims = [2, 3, 4, 5]
    for dim in dims:
        x = generate_data(dim)
        write_data_to_csv_file(x, f"densired_{dim}")

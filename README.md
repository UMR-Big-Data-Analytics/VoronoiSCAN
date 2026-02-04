# VoronoiSCAN

A distributed, reactive and exact DBSCAN implementation using Voronoi tessellations.

## Installing

VoronoiSCAN requires Scala 2.13 and Java 21 or higher. Further we require [qhull](http://qhull.org) to compute a
Delaunay graph and [kahip](https://kahip.github.io/) for node partitioning. Note this has only been tested on Linux and
MacOS systems.

To install qhull, you can clone the repository and build it from source:

```bash
git clone https://github.com/qhull/qhull.git
cd qhull
mkdir -p build
cd build
cmake ..
make
sudo make install
```

To install kahip, you can clone the repository and build it from source:

```bash
git clone https://github.com/KaHIP/KaHIP
cd KaHIP
./compile_withcmake.sh
```

Next, ensure that both `qhull` and `kahip` binaries are in your system's PATH and build the shared libraries. First, we
need [jextract](https://jdk.java.net/jextract/) to generate the Java bindings for the C libraries. You can install
jextract via SDKMAN:

```bash
sdk install jextract
```

Then generate the bindings:

```bash
./lib/makeKaHHIP.sh
./lib/makeQHull.sh
```

Since Akka changed the license model, you need to add the Akka repository for SBT. For this create an account
at [Akka webpage](https://account.akka.io/) to get your repository url and license key. Then, set `AKKA_SECURE_REPO_URL`
environment variable to the repository url and `AKKA_LICENSE_KEY` to the key.

Now, build the project using SBT:

```bash
sbt clean VoronoiSCAN/assembly
```

## Usage

For a single machine setup, you can run the following command:

```bash
java --enable-preview --add-modules jdk.incubator.vector --enable-native-access=ALL-UNNAMED -Djava.library.path=/lib -jar voronoiscan/target/scala-2.13/voronoiscan.jar Master -i <csv_path> -e <epsilon> -m <minPts> -c <numCells> -P <numPartitioners>  -w 1
```

For a distributed setup, you can run the following commands on different machines:

On the master node:

```bash
java --enable-preview --add-modules jdk.incubator.vector --enable-native-access=ALL-UNNAMED -Djava.library.path=/lib -jar voronoiscan/target/scala-2.13/voronoiscan.jar Master -i <csv_path> -e <epsilon> -m <minPts> -c <numCells> -P <numPartitioners>  -w <number_of_workers>
```

On each worker node:

```bash
java --enable-preview --add-modules jdk.incubator.vector --enable-native-access=ALL-UNNAMED -Djava.library.path=/lib -jar voronoiscan/target/scala-2.13/voronoiscan.jar Worker -p <some_port> -mh <master_host> -mp <master_port>
```

## Generating the densired_x datasets

To generate the densired_x datasets, you can use the provided Python script. First, ensure you have Python 3 installed
along with the required libraries. You can install the necessary libraries using pip:

```bash
pip install densired
```

Then, run the script to generate the datasets:

```bash
python3 densired_generator.py
```

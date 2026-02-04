#!/usr/bin/env bash

# Auto-detect QHULL_HOME if not set
if [ -z "$QHULL_HOME" ]; then
  if [ "$(uname)" == "Darwin" ]; then
    if [ -d "/opt/homebrew/opt/qhull" ]; then
      QHULL_HOME="/opt/homebrew/opt/qhull"
    elif [ -d "/usr/local/opt/qhull" ]; then
      QHULL_HOME="/usr/local/opt/qhull"
    fi
  elif [ "$(uname)" == "Linux" ]; then
    if [ -d "/usr/local/qhull" ]; then
      QHULL_HOME="/usr/local/qhull"
    elif [ -d "/opt/qhull" ]; then
      QHULL_HOME="/opt/qhull"
    elif [ -d "/usr/lib/qhull" ]; then
      QHULL_HOME="/usr/lib/qhull"
    fi
  fi
fi

if [ -z "$QHULL_HOME" ]; then
  echo "Error: QHULL_HOME is not set and Qhull installation could not be found automatically."
  echo "Please set QHULL_HOME manually or install Qhull in a standard location."
  exit 1
fi

echo "Using QHULL_HOME: $QHULL_HOME"

# Determine library extension based on OS
if [ "$(uname)" == "Darwin" ]; then
  LIB_EXT="dylib"
else
  LIB_EXT="so"
fi

gcc -fPIC -shared \
  -I${QHULL_HOME}/include \
  lib/qhull_wrapper.c \
  -L${QHULL_HOME}/lib -lqhull_r \
  -o lib/libqhull_wrapper.${LIB_EXT}

jextract \
  --output core/src/main/generated \
  -t org.qhull \
  -I ${QHULL_HOME}/include \
  -l :lib/libqhull_wrapper.${LIB_EXT} \
  lib/qhull_wrapper.h

echo "Current directory: $(pwd)"

cp lib/qhull_wrapper_h.java.template "core/src/main/generated/org/qhull/qhull_wrapper_h.java"
cp lib/qhull_wrapper_h_1.java.template "core/src/main/generated/org/qhull/qhull_wrapper_h_1.java"
cp lib/qhull_wrapper_h_2.java.template "core/src/main/generated/org/qhull/qhull_wrapper_h_2.java"
cp lib/_RuneLocale.java.template "core/src/main/generated/org/qhull/_RuneLocale.java"
cp lib/_RuneRange.java.template "core/src/main/generated/org/qhull/_RuneRange.java"


# On Linux find any string "lib/libqhull_wrapper.dylib" and rename to "lib/libqhull_wrapper.so"
if [ "$(uname)" == "Linux" ]; then
  find core/src/main/generated -type f -exec sed -i 's/libqhull_wrapper.dylib/libqhull_wrapper.so/g' {} +
fi

echo "Build library to lib/libqhull_wrapper.${LIB_EXT}"

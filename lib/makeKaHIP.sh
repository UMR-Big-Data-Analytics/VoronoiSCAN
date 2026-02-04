#!/usr/bin/env bash

# Auto-detect KAHIP_HOME if not set
if [ -z "$KAHIP_HOME" ]; then
  # Check common installation locations based on OS
  if [ "$(uname)" == "Darwin" ]; then
    # macOS - check Homebrew locations
    if [ -d "/opt/homebrew/opt/kahip/lib" ]; then
      KAHIP_HOME="/opt/homebrew/opt/kahip/lib"
    elif [ -d "/usr/local/opt/kahip/lib" ]; then
      KAHIP_HOME="/usr/local/opt/kahip/lib"
    fi
  elif [ "$(uname)" == "Linux" ]; then
    # Linux - check common locations
    if [ -d "/usr/local/kahip" ]; then
      KAHIP_HOME="/usr/local/kahip"
    elif [ -d "/opt/kahip" ]; then
      KAHIP_HOME="/opt/kahip"
    elif [ -d "/usr/lib/kahip" ]; then
      KAHIP_HOME="/usr/lib/kahip"
    fi
  fi
fi

# Fallback to default if still not found
if [ -z "$KAHIP_HOME" ]; then
  echo "Error: KAHIP_HOME is not set and KaHIP installation could not be found automatically."
  echo "Please set KAHIP_HOME manually or install KaHIP in a standard location."
  exit 1
fi

echo "Using KAHIP_HOME: $KAHIP_HOME"

# Determine library extension based on OS
if [ "$(uname)" == "Darwin" ]; then
  LIB_EXT="dylib"
else
  LIB_EXT="so"
fi

jextract \
  --output core/src/main/generated \
  -t org.kahip.kaffpa \
  -l :${KAHIP_HOME}/libkahip.${LIB_EXT} \
  lib/kahip_wrapper.h

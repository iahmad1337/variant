#!/usr/bin/env sh

if [ ! -f ./CMakeLists.txt ]
then
    echo "This script should be launched from the project root"
    exit 1
fi

additional_opts=""
build_dir=""
if [ "$1" = "debug" ]
then
    additional_opts="--preset=SanitizedDebug"
    build_dir="./cmake-build-SanitizedDebug"
elif [ "$1" = "release" ]
then
    additional_opts="--preset=Release"
    build_dir="./cmake-build-Release"
else
    echo "Please pass the build type (debug or release)"
    exit 1
fi

# NOTE: the second variable is not quoted because I want it to be split
cmake -S . "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON" "-DCMAKE_TOOLCHAIN_FILE=~/vcpkg/scripts/buildsystems/vcpkg.cmake" $additional_opts
cp "$build_dir/compile_commands.json" .


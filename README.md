# Sea Chess

Sea Chess is a chess game written in [C](https://www.c-language.org) with [raylib](https://www.raylib.com) for gui. Currently, it supports pseudo-legal move generation... Yes only that :3.

## Features

- Pseudo-legal move generation (legal move validation not yet implemented)

## Building from Source

### Prerequisites

- A C compiler (e.g., GCC or Clang)
- CMake (version 3.10 or higher recommended)

### Desktop

Use the following to build for desktop:

```sh
cmake -B build
cmake --build build
```

### Web

Compiling for the web requires the [Emscripten SDK](https://emscripten.org/docs/getting_started/downloads.html):

```bash
mkdir build
cd build
emcmake cmake .. -DPLATFORM=Web -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXECUTABLE_SUFFIX=".html"
emmake make
```

## Future Improvements

- Add Full legal move generation
- Add Checkmate and stalemate detection
- Add AI opponent (Chess bot)
- Add Time Controls

## License

This project is licensed under the MIT License.

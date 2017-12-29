# ships

My template for creating new haskell projects using cabal and nix.

## Initialization

First rename the project. This will go through and replace all the
ships text to your project name.

```
nix-shell --argstr project-name <your-project-name> nix/change-project-name.nix
```

This script generates the information necessary for pinning nixpkgs to a known
commit. Useful for reproducible builds. This tends to take a while because
the nixpkgs repo is pretty large. Should only need to run this once unless you
want to change to a different commit sha.

```
nix-shell --pure nix/generate-nixpkgs-json.nix
```


This script genereates the cabal file and nix file from the hpack yaml file.
Rerurn this whenever you update the hpack yaml file.

```
nix-shell --pure nix/generate-cabal-and-nix-file.nix
```

## Development

The purpose of this environment is to make available everything that your project
needs to compile, while leaving the project itself unbuilt. Then you can work on
your project and build it using cabal.

```
# Enter the development environment

nix-shell --pure nix/development.nix
```

```
cabal build
```

## Running Tests

```
# Enter the development environment

nix-shell --pure nix/development.nix
```

```
cabal test
```

## Release Build

The purpose of this environment is to allow nix to build your project and enable
you to test your executables or libraries the same way they would be installed
to your system or used by another project that depends on it.

```
# Enter the release environment

nix-shell --pure nix/release.nix
```


```
# Run your executable

ships
```

```
# See your library in the package list

ghc-pkg list
```

```
# Your library is available for use

λx. ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/srdqty/.ghc/ghci.conf

λ> import Lib

λ> helloWorld
"hello world!"
it :: String

λ>
```

## Docker Image

You can use nix to build a docker image for your project.

```
# Build the image. We don't use pure so we can use the system docker and nix-build.

nix-shell nix/build-docker-image.nix
```

```
# Run a container

docker run --rm ships-image
```

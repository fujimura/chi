chi
===========

Generate scaffold for a Haskell project

## Overview

This application generates a scaffold for Haskell project from a Git repository or Cabal package.

## Usage

### Basic Example

```
$ chi foo-bar-baz
Using hackage/chi-template-default as template.

Generated
  foo-bar-baz/.ghci                    from .ghci
  foo-bar-baz/.gitignore               from .gitignore
  foo-bar-baz/LICENSE                  from LICENSE
  foo-bar-baz/README.md                from README.md
  foo-bar-baz/Setup.hs                 from Setup.hs
  foo-bar-baz/foo-bar-baz.cabal        from package-name.cabal
  foo-bar-baz/src/Cli.hs               from src/Cli.hs
  foo-bar-baz/src/CommandLineOption.hs from src/CommandLineOption.hs
  foo-bar-baz/src/Main.hs              from src/Main.hs
  foo-bar-baz/src/Foo/Bar/Baz.hs       from src/ModuleName.hs
  foo-bar-baz/src/Option.hs            from src/Option.hs
  foo-bar-baz/src/Types.hs             from src/Types.hs
  foo-bar-baz/test/Foo/Bar/BazSpec.hs  from test/ModuleNameSpec.hs
  foo-bar-baz/test/Spec.hs             from test/Spec.hs
  foo-bar-baz/test/SpecHelper.hs       from test/SpecHelper.hs
  foo-bar-baz/test/doctests.hs         from test/doctests.hs
```

## Options

```
chi - Generate scaffold for a Haskell project

Usage: chi [-v|--version] [-m|--module-name ARG] [-d|--directory-name ARG]
           [-r|--repository ARG] [--after-command ARG] [-c|--cabal-package ARG]

Available options:
  -h,--help                Show this help text
  -v,--version             Print version information
  -m,--module-name ARG     Name of Module
  -d,--directory-name ARG  Directory to generate file
  -r,--repository ARG      Repository of template
  --after-command ARG      Command to run after generation
  -c,--cabal-package ARG   Name of cabal package
```

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request

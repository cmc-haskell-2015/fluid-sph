# fluid-sph

[![Build Status](https://travis-ci.org/cmc-haskell-2015/fluid-sph.svg?branch=master)](https://travis-ci.org/cmc-haskell-2015/fluid-sph)

Моделирование жидкости при помощи гидродинамики сглаженных частиц.

## Установка и запуск

Для установки клонируйте репозиторий и запустите `cabal install`:

```
$ git clone https://github.com/cmc-haskell-2015/fluid-sph.git
$ cd fluid-sph
$ cabal install
```

После установки запуск осуществляется командой `fluid-sph`:

```
$ fluid-sph
```

Для сборки и запуска текущей версии непосредственно из репозитория используйте `cabal run`:

```
$ cd fluid-sph
$ cabal run
```

## Документация

Автоматическая документация кода сгенерирована при помощи [Haddock](https://www.haskell.org/haddock/).

Онлайн документация доступна здесь: http://cmc-haskell-2015.github.io/fluid-sph/docs/

Локально документацию можно собрать, запустив простую команду:

```
$ cabal haddock
```


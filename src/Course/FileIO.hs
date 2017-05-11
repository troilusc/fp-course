{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ... 
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = do
  args <- getArgs
  case args of
    file :. Nil -> run file
    _ -> putStrLn "only one arg"

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run file = do
  contents <- readFile file
  files <- getFiles . lines $ contents
  printFiles files

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles = sequence . (getFile <$>)

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile f = do
  contents <- readFile f
  return (f, contents)

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles = void . sequence . ((\(path, contents) -> printFile path contents) <$>)

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile f c = do
  putStrLn ("============ " ++ f)
  putStrLn c


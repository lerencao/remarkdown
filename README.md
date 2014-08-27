## What

Remarkdown is going to be a parsing-combinator-based parser of an improved markdown syntax.
It follows most syntax of original markdown, but adds some clear, simple and unified rules.

## Why

Recently, I'm learning parsing combinator in compiler area, and I'd like to get some practice.
Problems about writing markdown comes to my mind.
So here is the idea.

## How

1. split the text into blocks, which are heading, code block, block quote, list, and so on.
2. for each block, do inline parse.
3. inline parsing parse inline spans like strong, link, bold, italics, and so on.
4. then result into corresponding html pieces.

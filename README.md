## What

Remarkdown is going to be a restricted and refined markdown parser based on scala parsing combinator.

*Restricted* means that it follows the markdown syntax,
but adds some clear, simple and unified rules to get rid of annoying misunderstanding of origin markdown.
while *Refined* means that it also contains some better and more convenient syntax instead of old ways. 

## Why

I love markdown because of its simple-to-use,
but I come to realize some faults that I dislike.
So I want to make a simplified and more strict text parser.

## How

I'm doing it using scala parsing combinator.

Here comes the approach:

1. split the text into blocks, which are heading, code block, block quote, list, and so on.
2. for each block, do inline parse.
3. inline parsing parse inline spans like strong, link, bold, italics, and so on.
4. then result into corresponding html pieces.

## Coding with me

If you are interested, please send issues to discuss it in detail.

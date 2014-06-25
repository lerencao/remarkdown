package me.remarkdown

/**
 * Block represents all kinds of block elems that remarkdown supports.
 * @author Jiafeng Cao
 * @since 06/23/2014
 */
sealed abstract class Block

case object Empty extends Block

case class Heading(level: Int, raw: String) extends Block

case object HorizontalRule extends Block

case class CodeBlock(raw: String) extends Block

case class BlockQuote(raw: String) extends Block

case class ListItem(raw: String)

case class List(kind: Boolean, ls: Seq[ListItem]) extends Block

case class Nested(raw: String)

case class Paragraph(raw: String) extends Block

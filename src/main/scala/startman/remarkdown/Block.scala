package startman.remarkdown

/**
 * Block represents all kinds of block elems that remarkdown supports.
 * @author Jiafeng Cao
 * @since 06/23/2014
 */
sealed abstract class Block
case object Empty extends Block
case object HorizontalRule extends Block
case class Heading(level: Int, var contents: Seq[Inline]) extends Block
case class Paragraph(var contents: Seq[Inline]) extends Block
case class CodeBlock(contents: String) extends Block
case class BlockQuote(var contents: Seq[Block]) extends Block
case class List(kind: Boolean, var contents: Seq[ListItem]) extends Block

case class ListItem(var contents: Seq[Block])


sealed abstract class Inline
case class PlainText(contents: String) extends Inline
case class Strong(contents: String) extends Inline
case class Underscore(contents: String) extends Inline
case class Italics(contents: String) extends Inline
case class Link(contents: String, url: String) extends Inline
case class InlineCode(contents: String) extends Inline

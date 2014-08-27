package startman.remarkdown

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Position, Reader}

/**
 * Reader for reading a stream of lines which contains no newlines.
 * @param lines sequence of string lines separated by newline(\n)
 *@param lineOffset current reader point
 */
class LineReader private(val lines: Seq[String], val lineOffset: Int) extends Reader[String] {

  def this(lines: Seq[String]) = this(lines, 0)

  override def first: String = lines(lineOffset)

  override def rest: Reader[String] =
    if (lineOffset < lines.length) new LineReader(lines, lineOffset + 1) else this

  override def atEnd: Boolean = lineOffset >= lines.length

  override def pos: Position = new Position {
    override def column: Int = 1

    override def line: Int = lineOffset

    override protected def lineContents: String = first
  }

}


/**
 * aggregate lines into chunks, which will be handled latter.
 */
trait LineAggregator extends Parsers {
  type Elem = String

  val $any: Parser[String] = acceptIf(line => true)(line => "this should not appear")

  // empty line
  val $emptyLine: Parser[Empty.type] = accept("") ^^^ Empty

  // heading
  val $heading: Parser[Heading] = Parser { in =>
    val pattern = """^(#{1,6})(.+)""".r
    if(in.atEnd) Failure("end of input", in)
    else in.first match {
      case pattern(l, s) => Success(Heading(l.length, s), in.rest)
      case _ => Failure("not a heading", in)
    }
  }

  // horizontal rule
  val $horizontalRule: Parser[HorizontalRule.type] =
    acceptIf(_.matches("""^={8,}"""))(line => "at least 8 equals") ^^^ HorizontalRule

  // block quote
  val $blockquoteMark: Parser[String] = accept(">>>")
  val $blockquote: Parser[BlockQuote] =
    ($blockquoteMark ~> rep(not($blockquoteMark) ~> $any) <~ $blockquoteMark) ^^ { ls =>
      BlockQuote(ls.mkString("\n"))
    }

  // code block
  val $codeblockMark: Parser[String] = accept("```")
  val $codeblock: Parser[CodeBlock] =
    ($codeblockMark ~> rep(not($codeblockMark) ~> $any) <~ $codeblockMark) ^^ { ls =>
      CodeBlock(ls.mkString("\n"))
    }

  // indented line, two or three spaces
  val $indentedLine: Parser[String] = acceptIf(line => line matches """^\s{2,3}.+""")(line => "un-indent line")

  // only support "-." as the starter of unordered list
  val $unorderedlistMark: Parser[String] =
    acceptIf(line => line matches """^-\..+""")(line => "not list item mark")
  val $unorderedlistitem: Parser[ListItem] =
    ($unorderedlistMark ~ rep($indentedLine | $emptyLine)) ^^ {
      case s ~ ss => ListItem(s + "\n" + ss.mkString("\n"))
    }
  val $unorderedList: Parser[List] =
    rep1($unorderedlistitem) ^^ { items => List(kind = true, items) }

  // only support "[number]." as the starter of ordered list
  val $orderedlistMark: Parser[String] =
    acceptIf(line => line matches """^[1-9][0-9]*\..+""")(line => "not list item mark")
  val $orderedlistItem: Parser[ListItem] =
    ($orderedlistMark ~ rep($indentedLine | $emptyLine)) ^^ {
      case s ~ ss => ListItem(s + "\n" + ss.mkString("\n"))
    }
  val $orderedList: Parser[List] =
    rep1($orderedlistItem) ^^ { items => List(kind = false, items) }

  // normal line, to compose a paragraph
  val $normalLine: Parser[String] =
    (
      not($emptyLine)         ~
      not($horizontalRule)    ~
      not($heading)           ~
      not($blockquoteMark)    ~
      not($codeblockMark)     ~
      not($unorderedlistMark) ~
      not($orderedlistMark)   ~
      not($indentedLine)
    ) ~> $any

  // paragraph
  val $paragraph: Parser[Paragraph] = rep1($normalLine) ^^ { ls => Paragraph(ls.mkString) }


  val $block: Parser[Block] =
    $emptyLine      |
    $horizontalRule |
    $heading        |
    $blockquote     |
    $codeblock      |
    $unorderedList  |
    $orderedList    |
    $paragraph

  /**
   * group lines into blocks
   * @param in a reader of line streams
   */
  def group(in: Reader[String]) = phrase($block.*)(in)

}

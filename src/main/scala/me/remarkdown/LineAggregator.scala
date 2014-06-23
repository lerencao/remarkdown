package me.remarkdown

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

  val $emptyLine: Parser[Elem] = accept("")

  /**
   * heading
   */
  val $heading: Parser[Elem] =
    Parser { in: Reader[Elem] =>
      if (in.atEnd) Failure("end of reader", in)
      else if (in.first matches """^#{1,6}.+""") Success(in.first, in.rest)
      else Failure("not a heading", in)
    }

  val $horizontalRule: Parser[Elem] = acceptIf(_.matches("""^={8,}"""))(line => "at least 8 equals")

  val $any: Parser[Elem] =
    Parser { in: Reader[Elem] =>
      if (in.atEnd) Failure("end of input", in)
      else Success(in.first, in.rest)
    }

  /**
   * block quote
   */
  val $blockquoteStart: Parser[Elem] = accept(">>>")
  val $blockquote: Parser[Elem] =
    ($blockquoteStart ~ rep(not($blockquoteStart) ~> $any) ~ $blockquoteStart) ^^ {
      case s1 ~ ss ~ s2 => s1 + "\n" + ss.mkString("\n") + "\n" + s2
    }

  /**
   * code block
   */
  val $codeblockStart: Parser[Elem] = accept("```")
  val $codeblock: Parser[Elem] =
    ($codeblockStart ~ rep(not($codeblockStart) ~> $any) ~ $codeblockStart) ^^ {
      case s1 ~ ss ~ s2 => s1 + "\n" + ss.mkString("\n") + "\n" + s2
    }

  /**
   * only support "-." as the starter of unordered list
   */
  val $unorderedlistMark: Parser[Elem] = acceptIf(line => line matches """^-\..+""")(line => "not list item mark")
  val $indentedLine: Parser[Elem] = acceptIf(line => line matches """^\s{2,3}.+""")(line => "un-indent line")
  val $unorderedlistitem: Parser[Elem] =
    ($unorderedlistMark ~ rep($indentedLine | $emptyLine)) ^^ {
      case s ~ ss => s + "\n" + ss.mkString("\n")
    }
  val $unorderedList: Parser[Elem] =
    rep1($unorderedlistitem) ^^ { _.mkString("\n\n") }

  /**
   * only support "[number]." as the starter of ordered list
   */
  val $orderedlistMark: Parser[Elem] =
    acceptIf(line => line matches """^[1-9][0-9]*\..+""")(line => "not list item mark")
  val $orderedlistItem: Parser[Elem] =
    ($orderedlistMark ~ rep($indentedLine | $emptyLine)) ^^ {
      case s ~ ss => s + "\n" + ss.mkString("\n")
    }
  val $orderedList: Parser[Elem] =
    rep1($orderedlistItem) ^^ (_.mkString("\n\n"))

  /**
   * paragraph
   */
  val $paragraph: Parser[Elem] =
    rep1((not($emptyLine) ~ not($heading) ~ not($blockquote) ~ not($codeblock)) ~> $any) ^^ (_.mkString)


  val $block: Parser[Elem] =
    $emptyLine | $heading | $horizontalRule | $blockquote | $codeblock |  $unorderedList | $orderedList | $paragraph

  /**
   * group lines into blocks
   * @param in a reader of line streams
   */
  def group(in: Reader[Elem]) = phrase($block.*)(in)

}

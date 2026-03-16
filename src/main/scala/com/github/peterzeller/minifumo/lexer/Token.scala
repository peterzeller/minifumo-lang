package com.github.peterzeller.minifumo.lexer

import com.github.peterzeller.minifumo.ast.{SourcePos, SourceRange}

/** Represents the kind of lexical token produced by the handwritten lexer. */
enum TokenKind:
  case EOF, NL, BEGIN, END, SPACETAB
  case ID, INT, BOOL, STRING
  case IMPORT, FROM, IN, EXPORT, DATA, FUN, LEMMA, GIVEN, ASSUMES, SHOWS, PROOF, MATCH, CASE, IF, THEN, ELSE, LET, FORALL, EXISTS, AXIOM
  case UNDERSCORE, HOLE
  case PAREN_LEFT, PAREN_RIGHT, BRACKET_LEFT, BRACKET_RIGHT, BRACE_LEFT, BRACE_RIGHT
  case DOT, COMMA, PLUS, MULT, MINUS, DIV, MOD, AND, OR, NOT, META_AND, META_OR, META_NOT
  case COLON, COLONCOLON, EQ, EQEQ, NOTEQ, BAR
  case LT, LE, GT, GE
  case IMPLIES, IFF
  case ARROW, FAT_ARROW

/** Stores one token with source coordinates and original text. */
case class Token(kind: TokenKind, text: String, source: SourceRange, commentBefore: String = "")

object Token:
  /** Creates an EOF token at a single source position. */
  def eof(pos: SourcePos): Token =
    Token(TokenKind.EOF, "$EOF", SourceRange(pos, pos))

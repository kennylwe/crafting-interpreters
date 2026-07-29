package com.craftinginterpreters.lox;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.craftinginterpreters.lox.TokenType.*;

class Scanner {
  private final String source;
  private final List<Token> tokens = new ArrayList<>();
  private int start;
  private int current;
  private int line = 1;

  Scanner(String source) {
    this.source = source;
  }

  List <Token> scanTokens() {
    while (!isAtEnd()) {
      //we are now at the beginning of the next lexeme
      start = current;
      scanToken();
    }
    tokens.add(new Token(EOF, "", null, line));
    return tokens;
  }

  private boolean isAtEnd() {
    return current >= source.length();
  }

}



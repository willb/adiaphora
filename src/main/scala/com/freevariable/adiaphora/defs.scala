/*
 * Copyright (c) 2015 William C. Benton and Red Hat, Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.freevariable.adiaphora

trait OptionProcessing[Options <: Product] {
  type Result = Triple[Options, List[String], List[String]]
  type Matcher = PartialFunction[Result, Result]
  
  val base: Matcher = {
    case r @ (_, _, Nil) => r
  }
  
  val defaults: Matcher = {
    case (opts, args, "--" :: rest) => (opts, rest.reverse ++ args, Nil)
    case (opts, args, bogusOpt) if bogusOpt(0) == "-" => throw new RuntimeException(s"unrecognized option $bogusOpt")
    case (opts, args, arg :: rest) => (opts, arg :: args, rest)
  }
  
  def optionMatchers: List[Matcher] = Nil
  
  def emptyOptions: Options
  
  lazy val optionMatcher: Matcher = (base :: optionMatchers).foldRight(defaults)((a, b) => a orElse b)
  
  def parse(args: Array[String]): Pair[Options, List[String]] = {
    saturate(Triple(emptyOptions, List[String](), args.toList)) match {
      case (opts, params, _) => (opts, params.reverse)
    }
  }
  
  private def saturate(r: Result): Result = {
    optionMatcher(r) match {
      case x if x == r => r
      case y => saturate(y)
    }
  }
}

class BasicOptionProcessor[Options <: Product](
  empty: Options, 
  handler: PartialFunction[Triple[Options, List[String], List[String]], Triple[Options, List[String], List[String]]]
  ) extends OptionProcessing[Options] {
  override def optionMatchers = List(handler)
  override def emptyOptions = empty
}

object Basic {
  def apply[Options <: Product](empty: Options)(handler: PartialFunction[Triple[Options, List[String], List[String]], Triple[Options, List[String], List[String]]]): BasicOptionProcessor[Options] = {
    new BasicOptionProcessor(empty, handler)
  }
}
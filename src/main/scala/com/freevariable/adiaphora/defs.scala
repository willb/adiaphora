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
 * limitations under the License.c
 */

package com.freevariable.adiaphora

trait OptionProcessor[Options <: Product] {
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
  
  lazy val optionMatcher: Matcher = (base :: optionMatchers).foldRight(defaults)((a, b) => a orElse b)
  
  private def saturate(r: Result): Result = {
    optionMatcher(r) match {
      case x if x == r => r
      case y => saturate(y)
    }
  }
}
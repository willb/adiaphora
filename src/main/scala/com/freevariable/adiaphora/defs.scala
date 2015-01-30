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
  case class Result(options: Options, args: List[String], inputs: List[String]) {}
  type Matcher = PartialFunction[Result, Result]
  
  val base: Matcher = {
    case r @ Result(_, _, Nil) => r
  }
  
  val defaults: Matcher = {
    case r @ Result(opts, args, "--" :: rest) => Result(opts, args ++ rest, Nil)
    case r @ Result(opts, args, bogusOpt) if bogusOpt(0) == "-" => throw new RuntimeException(s"unrecognized option $bogusOpt")
    case r @ Result(opts, args, arg :: rest) => Result(opts, args ++ List(arg), Nil)
  }
  
  def optionMatchers: List[Matcher] = Nil
  
  def optionMatcher: Matcher = (base :: optionMatchers).foldRight(defaults)((a, b) => a orElse b)
}
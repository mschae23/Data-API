package de.martenschaefer.data.test

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

abstract class UnitSpec extends AnyFlatSpec, should.Matchers, OptionValues, Inside, Inspectors

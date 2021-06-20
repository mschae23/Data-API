package de.martenschaefer.data.serialization

import java.io.{ StringReader, StringWriter }
import java.util.Properties
import scala.annotation.tailrec
import scala.jdk.CollectionConverters
import de.martenschaefer.data.util.Either._
import de.martenschaefer.data.util.{ Either, Utils }

object PropertiesCodecs {
    given propertiesEncoder: Encoder[Element, String] with {
        override def encode(element: Element): Result[String] = {
            val properties = new Properties()

            element match {
                case Element.ObjectElement(map) =>
                    this.encodeObject(properties, map, "")

                case _ => return Left(Vector(RecordParseError.NotAnObject(element, List())))
            }

            val writer = new StringWriter()
            properties.store(writer, null)
            Right(writer.toString())
        }

        def encodeObject(properties: Properties, map: Map[String, Element], namePrefix: String): Unit = {
            for ((name, fieldElement) <- map) {
                val key = namePrefix + name

                this.encode(properties, fieldElement, key)
            }
        }

        def encodeArray(properties: Properties, elements: List[Element], name: String): Unit = {
            for (i <- 0 until elements.size) {
                this.encode(properties, elements(i), name + i)
            }
        }

        def encode(properties: Properties, element: Element, name: String): Unit = {
            element match {
                case Element.Null => properties.setProperty(name, "null")
                case Element.None =>
                case Element.IntElement(value) => properties.setProperty(name, value.toString)
                case Element.LongElement(value) => properties.setProperty(name, value.toString)
                case Element.FloatElement(value) => properties.setProperty(name, value.toString)
                case Element.DoubleElement(value) => properties.setProperty(name, value.toString)
                case Element.BooleanElement(value) => properties.setProperty(name, value.toString)
                case Element.StringElement(value) => properties.setProperty(name, value)

                case Element.ArrayElement(values) => this.encodeArray(properties, values, name + ".")

                case Element.ObjectElement(fields) => this.encodeObject(properties, fields, name + ".")
            }
        }
    }

    import CollectionConverters.SetHasAsScala

    given propertiesDecoder: Decoder[Element, String] with {
        override def decode(encoded: String): Result[Element] = {
            val properties = new Properties()
            properties.load(new StringReader(encoded))

            var element = Element.ObjectElement(Map())

            for (key <- properties.stringPropertyNames().asScala) {
                val value = properties.getProperty(key)
                val path = List.from(key.split("\\."))

                for (pathPart <- path) {
                    element match {
                        case Element.ObjectElement(map) =>
                            element = Element.ObjectElement(this.updateMap(map, path, value))

                        case _ => return Left(Vector(RecordParseError.NotAnObject(element, List())))
                    }
                }
            }

            Right[Vector[ElementError], Element](element)
        }

        def updateMap(map: Map[String, Element], path: List[String], value: String): Map[String, Element] = {
            if (path.size == 1)
                map.updated(path(0), Utils.parsePrimitive(value))
            else {
                val newMap = map.get(path(0)).map(_ match {
                    case Element.ObjectElement(fields) => fields
                    case _ => throw new IllegalArgumentException(path(0) + " is not an object")
                }).getOrElse(Map())

                map.updated(path(0), Element.ObjectElement(updateMap(newMap, path.tail, value)))
            }
        }
    }
}

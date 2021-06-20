package de.martenschaefer.data.serialization.codec

import scala.collection.immutable.ListMap
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode, RecordParseError, Result }
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.util.{ Lifecycle, Utils }
import de.martenschaefer.data.util.Either._
import shapeless3.deriving.{ K0, Labelling }

class DerivedCodec[A](using inst: K0.ProductInstances[Codec, A], labelling: Labelling[A]) extends Codec[A] {
    override def encodeElement(value: A): Result[Element] =
        if (labelling.elemLabels.isEmpty)
            Right(ObjectElement(ListMap.empty))
        else
            labelling.elemLabels.zipWithIndex.map(
                (label, i) => inst.project(value)(i)([T] => (codec: Codec[T], t: T) => (label, codec.encodeElement(t)))
            ).foldLeft[Result[Map[String, Element]]](Right(ListMap.empty)) { (acc, tuple) =>
                val label = Utils.toSnakeCase(tuple._1)
                val result = tuple._2

                result match {
                    case Right(element) =>
                        acc.map(_.updated(label, element))
                    case Left(errors) =>
                        acc.mapLeft(_.appendedAll(errors.map(_.withPrependedPath(label))))
                }
            }.map(ObjectElement(_))

    override def decodeElement(element: Element): Result[A] = element match {
        case ObjectElement(map) =>
            type Acc = (Seq[String], Vector[ElementError])

            inst.unfold[Acc]((labelling.elemLabels, Vector()))([t] => (acc: Acc, codec: Codec[t]) => {
                val (elemLabels, errors) = acc
                val last = elemLabels.size < 2
                // Don't return None before the last field, as that makes it not decode every element
                val none: Option[t] = if (last) scala.None else Some(null.asInstanceOf[t])
                val label = Utils.toSnakeCase(elemLabels.head)
                val fieldElement = map.get(label).getOrElse(None)

                codec.decodeElement(fieldElement) match {
                    case Right(value) => if (!last) ((elemLabels.tail, errors), Some(value)) else ((elemLabels.tail, errors),
                        if (errors.isEmpty) Some(value) else scala.None)

                    case Left(errors2) => if (fieldElement == None) ((elemLabels.tail, errors.appended(
                        RecordParseError.MissingKey(element, List(ElementNode.Name(label))))), none) else
                        ((elemLabels.tail, errors.appendedAll(errors2.map(_.withPrependedPath(label)))), none)
                }
            }) match {
                case ((_, errors), scala.None) => Left(errors)
                case (_, Some(value)) => Right(value)
            }

        case _ => Left(Vector(RecordParseError.NotAnObject(element, List())))
    }

    private type LifecycleAcc = (Lifecycle, Int)

    override val lifecycle: Lifecycle = inst.unfold[LifecycleAcc]((Lifecycle.Stable, labelling.elemLabels.size))(
        [t] => (acc: LifecycleAcc, codec: Codec[t]) => {
            val (lifecycle, fields) = acc
            val last = fields < 2
            val option: Option[t] = if (last) scala.None else Some(null.asInstanceOf[t])

            ((lifecycle + codec.lifecycle, fields - 1), option)
        }) match {
        case ((lifecycle, _), _) => lifecycle
    }
}

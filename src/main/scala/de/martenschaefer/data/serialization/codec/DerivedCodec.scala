package de.martenschaefer.data.serialization.codec

import scala.collection.immutable.ListMap
import de.martenschaefer.data.Result
import de.martenschaefer.data.serialization.{ Codec, Element, ElementError, ElementNode, RecordParseError }
import de.martenschaefer.data.serialization.Element._
import de.martenschaefer.data.util.{ Lifecycle, Utils }
import de.martenschaefer.data.util.DataResult._
import shapeless3.deriving.{ K0, Labelling }

class DerivedCodec[A](using inst: K0.ProductInstances[Codec, A], labelling: Labelling[A]) extends Codec[A] {
    override def encodeElement(value: A): Result[Element] =
        if (labelling.elemLabels.isEmpty)
            Success(ObjectElement(ListMap.empty), this.lifecycle)
        else
            labelling.elemLabels.zipWithIndex.map(
                (label, i) => inst.project(value)(i)([T] => (codec: Codec[T], t: T) => (label, codec.encodeElement(t)))
            ).foldLeft[Result[Map[String, Element]]](Success(ListMap.empty, Lifecycle.Stable)) { (acc, tuple) =>
                val label = Utils.toSnakeCase(tuple._1)
                val result = tuple._2

                result match {
                    case Success(element, l) =>
                        acc.flatMap(map => Success(map.updated(label, element), acc.lifecycle + l))
                    case Failure(errors, l) =>
                        acc.mapLeft(errors.map(_.withPrependedPath(label)) ::: _).addLifecycle(l)
                }
            }.map(ObjectElement(_))

    override def decodeElement(element: Element): Result[A] = element match {
        case ObjectElement(map) =>
            type Acc = (Seq[String], List[ElementError], Lifecycle)

            inst.unfold[Acc]((labelling.elemLabels, List.empty, Lifecycle.Stable))([t] => (acc: Acc, codec: Codec[t]) => {
                val (elemLabels, errors, lifecycle) = acc
                val last = elemLabels.size < 2
                // Don't return None before the last field, as that makes it not decode every element
                val none: Option[t] = if (last) scala.None else Some(null.asInstanceOf[t])
                val label = Utils.toSnakeCase(elemLabels.head)
                val fieldElement = map.get(label).getOrElse(None)

                codec.decodeElement(fieldElement) match {
                    case Success(value, l) =>
                        if (!last) ((elemLabels.tail, errors, lifecycle + l), Some(value))
                        else ((elemLabels.tail, errors, lifecycle + l),
                            if (errors.isEmpty) Some(value) else scala.None)

                    case Failure(errors2, l) => if (fieldElement == None) ((elemLabels.tail,
                        errors.appended(RecordParseError.MissingKey(element, List(ElementNode.Name(label)))), lifecycle + l), none) else
                        ((elemLabels.tail, errors ::: errors2.map(_.withPrependedPath(label)), lifecycle + l), none)
                }
            }) match {
                case ((_, errors, lifecycle), scala.None) => Failure(errors, lifecycle)
                case ((_, _, lifecycle), Some(value)) => Success(value, lifecycle)
            }

        case _ => Failure(List(RecordParseError.NotAnObject(element, List.empty)), this.lifecycle)
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

package de.martenschaefer.data

import de.martenschaefer.data.serialization.ElementError
import de.martenschaefer.data.util.DataResult

/**
 * Can be used for errors.
 * Will be either a {@link List} of errors, or an object of type {@code T}.
 *
 * @tparam T Type of object in case of no errors.
 */
type Result[+T] = DataResult[List[ElementError], T]

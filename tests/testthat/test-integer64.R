test_that("integer64 coercion to/from other types work", {
  # from integer64
  expect_identical(as.logical(as.integer64(0:1)), c(FALSE, TRUE))
  expect_identical(as.integer(as.integer64(1:10)), 1:10)
  expect_identical(as.character(as.integer64(1:10)), as.character(1:10))
  expect_identical(as.double(as.integer64(1:10)), as.double(1:10))
  expect_identical(as.numeric(as.integer64(1:10)), as.numeric(1:10))
  expect_identical(as.complex(as.integer64(1:10)), as.complex(1:10))
  expect_identical(as.raw(as.integer64(1:10)), as.raw(1:10))
  expect_identical(as.factor(as.integer64(1:10)), as.factor(1:10))
  expect_identical(as.ordered(as.integer64(1:10)), as.ordered(1:10))

  # to integer64
  expect_identical(as.integer64(TRUE), as.integer64(1L))
  expect_identical(as.integer64(as.character(1:10)), as.integer64(1:10))
  expect_identical(as.integer64(as.double(1:10)), as.integer64(1:10))
  expect_identical(as.integer64(as.complex(1:10)), as.integer64(1:10))
  expect_identical(as.integer64(as.raw(1:10)), as.integer64(1:10))
  expect_identical(as.integer64(as.factor(11:20)), as.integer64(1:10))
  expect_identical(as.integer64(as.ordered(11:20)), as.integer64(1:10))
  expect_identical(as.integer64(NULL), as.integer64())
  x = as.integer64(1:10)
  expect_identical(as.integer64(x), x)

  # S4 version
  expect_identical(methods::as(as.character(1:10), "integer64"), as.integer64(1:10))
  expect_identical(methods::as(as.factor(11:20), "integer64"), as.integer64(1:10))
  expect_identical(methods::as(as.ordered(11:20), "integer64"), as.integer64(1:10))
  expect_warning(expect_identical(methods::as(as.complex(1:10) + 0+1i, "integer64"), as.integer64(1:10)), "imaginary parts discarded in coercion")
  expect_identical(methods::as(as.numeric(1:10), "integer64"), as.integer64(1:10))
  expect_identical(methods::as(as.integer(1:10), "integer64"), as.integer64(1:10))
  expect_identical(methods::as(as.raw(1:10), "integer64"), as.integer64(1:10))
  expect_identical(methods::as(as.logical(0:2), "integer64"), as.integer64(c(0L, 1L, 1L)))
  expect_identical(methods::as(as.integer64(1:10), "character"), as.character(1:10))
  expect_identical(methods::as(as.integer64(1:10), "factor"), as.factor(1:10))
  expect_identical(methods::as(as.integer64(1:10), "ordered"), as.ordered(1:10))
  expect_identical(methods::as(as.integer64(1:10), "complex"), as.complex(1:10))
  expect_identical(methods::as(as.integer64(1:10), "numeric"), as.numeric(1:10))
  expect_identical(methods::as(as.integer64(1:10), "integer"), as.integer(1:10))
  expect_identical(methods::as(as.integer64(1:10), "raw"), as.raw(1:10))
  expect_identical(methods::as(as.integer64(1:10), "logical"), as.logical(1:10))

  # now for NA
  expect_identical(as.logical(NA_integer64_), NA)
  expect_identical(as.integer(NA_integer64_), NA_integer_)
  expect_identical(as.double(NA_integer64_), NA_real_)
  expect_identical(as.character(NA_integer64_), NA_character_)
  expect_identical(as.integer64(NA), NA_integer64_)
  expect_identical(as.integer64(NA_integer_), NA_integer64_)
  expect_identical(as.integer64(NA_real_), NA_integer64_)
  expect_identical(as.integer64(NA_character_), NA_integer64_)
})

test_that("S3 class basics work", {
  x = as.integer64(1:10)
  expect_s3_class(x, "integer64")
  expect_true(is.integer64(x))

  length(x) = 11L
  expect_length(x, 11L)
  expect_identical(x[11L], as.integer64(0L))
})

test_that("indexing works", {

  x = as.integer64(1:10)
  x[1.0] = 2L
  x[2L] = 3L
  expect_identical(x, as.integer64(c(2:3, 3:10)))

  x = as.integer64(1:10)
  x[2L] = 3L
  x[1.0] = 2.0
  expect_identical(x, as.numeric(c(2:3, 3:10)))

  x = as.integer64(1:10)
  x[[1.0]] = 3L
  x[[2L]] = 4L
  expect_identical(x, as.integer64(c(3:4, 3:10)))

  x = as.integer64(1:10)
  x[[2L]] = 4L
  x[[1.0]] = 3.0
  expect_identical(x, as.numeric(c(3:4, 3:10)))

  x = as.integer64(1:10)
  expect_identical(x[3L], as.integer64(3L))
  expect_identical(x[[4L]], as.integer64(4L))

  names(x) = letters[1:10]
  expect_identical(x[c("b", "c")], x[2:3])
  expect_identical(x[["d"]], x[[4L]])
  
  expect_no_warning(expect_identical(integer64()[integer()], integer64()))
  expect_no_warning(expect_identical(structure(as.integer64(1L), dim=c(1))[1], as.integer64(1L)))
})

test_that("arithmetic & basic math works", {
  x = as.integer64(1:10)
  y = as.integer64(10:1)

  expect_identical(x + y, as.integer64(rep(11L, 10L)))
  expect_identical(y - x, as.integer64(seq(9L, -9L, by=-2L)))
  expect_identical(x * y, as.integer64(c(10L, 18L, 24L, 28L, 30L, 30L, 28L, 24L, 18L, 10L)))
  # output is double even though it fits in integer [and integer64]
  expect_identical(x[seq(2L, 10L, by=2L)] / 2L, as.double(1:5))
  expect_identical(x ^ 2L, as.integer64((1:10)^2L))
  expect_identical(-x, as.integer64(-(1:10)))

  expect_identical(x %/% 2L, as.integer64(c(0L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L)))
  expect_identical(x %% 2L, as.integer64(rep_len(c(1L, 0L), 10L)))

  expect_identical(sign(x - 6L), as.integer64(rep(c(-1L, 0L, 1L), c(5L, 1L, 4L))))
  expect_identical(abs(x - 6.0), as.integer64(c(5:0, 1:4)))

  expect_identical(sqrt(as.integer64(c(0L, 1L, 4L, 9L))), as.numeric(0:3))
  expect_identical(log(x), log(as.numeric(x)))
  expect_identical(log(as.integer64(c(1L, 2L, 4L, 8L)), base=2L), as.numeric(0:3))
  expect_identical(log2(as.integer64(c(1L, 2L, 4L, 8L))), as.numeric(0:3))
  # TODO(#48): Improve the numerical precision here.
  expect_identical(log10(as.integer64(c(1L, 10L, 100L, 1000L))), as.numeric(0:3), tolerance=1e-7)

  expect_identical(trunc(x), x)
  expect_identical(floor(x), x)
  expect_identical(ceiling(x), x)
  expect_identical(signif(x), x)
  expect_identical(round(x), x)

  expect_identical(round(x, -1L), as.integer64(rep(c(0L, 10L), each=5L)))

  # regression snuck through, caught by #149
  expect_identical(as.integer64(1L) * 1:5, as.integer64(1:5))
  expect_identical(1:5 * as.integer64(1L), as.integer64(1:5))
})

test_that("basic statistics work", {
  x = as.integer64(1:10)

  expect_identical(sum(integer64()), as.integer64(0L))
  expect_identical(sum(NA_integer64_), NA_integer64_)
  expect_identical(sum(NA_integer64_, na.rm=TRUE), as.integer64(0L))
  expect_identical(sum(x), as.integer64(55L))
  expect_identical(sum(x, x), as.integer64(110L))
  expect_identical(sum(c(x, NA)), NA_integer64_)
  expect_identical(sum(c(x, NA), na.rm=TRUE), as.integer64(55L))
  expect_identical(sum(x, as.integer64(45L)), as.integer64(100L))
  expect_identical(sum(c(x, NA), as.integer64(c(45L, NA))), NA_integer64_)
  expect_identical(sum(c(x, NA), as.integer64(c(45L, NA)), na.rm=TRUE), as.integer64(100L))
  expect_identical(sum(x, NA), NA_integer64_)
  expect_identical(sum(x, NA, na.rm=TRUE), as.integer64(55L))
  expect_identical(sum(x, as.logical(c(45, NA))), NA_integer64_)
  expect_identical(sum(x, as.logical(c(45, NA)), na.rm=TRUE), as.integer64(56L))
  expect_identical(sum(x, NA_integer_), NA_integer64_)
  expect_identical(sum(x, NA_integer_, na.rm=TRUE), as.integer64(55L))
  expect_identical(sum(x, as.integer(c(45, NA))), NA_integer64_)
  expect_identical(sum(x, as.integer(c(45, NA)), na.rm=TRUE), as.integer64(100L))
  expect_identical(sum(x, NA_real_), NA_real_)
  expect_identical(sum(x, NA_real_, na.rm=TRUE), 55)
  expect_identical(sum(x, as.numeric(c(45, NA))), NA_real_)
  expect_identical(sum(x, as.numeric(c(45, NA)), na.rm=TRUE), 100)
  expect_identical(sum(x, NA_complex_), NA_complex_)
  expect_identical(sum(x, NA_complex_, na.rm=TRUE), 55+0i)
  # expect_identical(sum(x, as.complex(c(45, NA))), NA_complex_) # for some odd reason this doesn't work
  tmp = sum(x, as.complex(c(45, NA)))
  expect_true(is.complex(tmp) && is.na(tmp))
  expect_identical(sum(x, as.complex(c(45, NA)), na.rm=TRUE), 100+0i)
  expect_error(sum(x, NA_character_), "invalid 'type' (character) of argument", fixed=TRUE)
  expect_error(sum(x, NA_character_, na.rm=TRUE), "invalid 'type' (character) of argument", fixed=TRUE)
  expect_error(sum(x, character()), "invalid 'type' (character) of argument", fixed=TRUE)
  expect_error(sum(x, character(), na.rm=TRUE), "invalid 'type' (character) of argument", fixed=TRUE)
  expect_error(sum(x, list()), "invalid 'type' (list) of argument", fixed=TRUE)
  expect_error(sum(x, list(), na.rm=TRUE), "invalid 'type' (list) of argument", fixed=TRUE)
  
  expect_identical(prod(integer64()), as.integer64(1L))
  expect_identical(prod(NA_integer64_), NA_integer64_)
  expect_identical(prod(NA_integer64_, na.rm=TRUE), as.integer64(1L))
  expect_identical(prod(x), as.integer64(factorial(10L)))
  expect_identical(prod(x[1:5], x[6:10]), as.integer64(factorial(10L)))
  expect_identical(prod(c(x, NA)), NA_integer64_)
  expect_identical(prod(c(x, NA), na.rm=TRUE), as.integer64(factorial(10L)))
  expect_identical(prod(x, as.integer64(45L)), as.integer64(163296000L))
  expect_identical(prod(c(x, NA), as.integer64(c(45L, NA))), NA_integer64_)
  expect_identical(prod(c(x, NA), as.integer64(c(45L, NA)), na.rm=TRUE), as.integer64(163296000L))
  expect_identical(prod(x, NA), NA_integer64_)
  expect_identical(prod(x, NA, na.rm=TRUE), as.integer64(factorial(10L)))
  expect_identical(prod(x, as.logical(c(45, NA))), NA_integer64_)
  expect_identical(prod(x, as.logical(c(45, NA)), na.rm=TRUE), as.integer64(factorial(10L)))
  expect_identical(prod(x, NA_integer_), NA_integer64_)
  expect_identical(prod(x, NA_integer_, na.rm=TRUE), as.integer64(factorial(10L)))
  expect_identical(prod(x, as.integer(c(45, NA))), NA_integer64_)
  expect_identical(prod(x, as.integer(c(45, NA)), na.rm=TRUE), as.integer64(163296000L))
  expect_identical(prod(x, NA_real_), NA_real_)
  expect_identical(prod(x, NA_real_, na.rm=TRUE), factorial(10))
  expect_identical(prod(x, as.numeric(c(45, NA))), NA_real_)
  expect_identical(prod(x, as.numeric(c(45, NA)), na.rm=TRUE), 163296000)
  expect_identical(prod(x, NA_complex_), NA_complex_)
  expect_identical(prod(x, NA_complex_, na.rm=TRUE), as.complex(factorial(10)))
  expect_identical(prod(x, as.complex(c(45, NA))), NA_complex_)
  expect_identical(prod(x, as.complex(c(45, NA)), na.rm=TRUE), 163296000+0i)
  expect_error(prod(x, NA_character_), "invalid 'type' (character) of argument", fixed=TRUE)
  expect_error(prod(x, NA_character_, na.rm=TRUE), "invalid 'type' (character) of argument", fixed=TRUE)
  expect_error(prod(x, character()), "invalid 'type' (character) of argument", fixed=TRUE)
  expect_error(prod(x, character(), na.rm=TRUE), "invalid 'type' (character) of argument", fixed=TRUE)
  expect_error(prod(x, list()), "invalid 'type' (list) of argument", fixed=TRUE)
  expect_error(prod(x, list(), na.rm=TRUE), "invalid 'type' (list) of argument", fixed=TRUE)
  
  expect_identical(min(x), x[1L])
  expect_identical(min(x, as.integer64(0L)), as.integer64(0L))
  expect_warning(expect_identical(min(integer64()), lim.integer64()[2L]), "no non-NA value")
  expect_warning(expect_identical(min(integer64(), na.rm=TRUE), lim.integer64()[2L]), "no non-NA value")
  expect_no_warning(expect_identical(min(x, integer64()), x[1L]))
  expect_no_warning(expect_identical(min(x, integer64(), na.rm=TRUE), x[1L]))
  expect_no_warning(expect_identical(min(lim.integer64()[2L]), lim.integer64()[2L]))
  expect_no_warning(expect_identical(min(lim.integer64()[2L], NA_integer64_, na.rm=TRUE), lim.integer64()[2L]))
  expect_no_warning(expect_identical(min(integer64(), NA_integer64_), NA_integer64_))
  expect_warning(expect_identical(min(integer64(), NA_integer64_, na.rm=TRUE), lim.integer64()[2L]), "no non-NA value")
  expect_no_warning(expect_identical(min(integer64(1), NA_integer64_), NA_integer64_))
  expect_no_warning(expect_identical(min(integer64(1), NA_integer64_, na.rm=TRUE), as.integer64(0L)))
  expect_no_warning(expect_identical(min(integer64(1), integer64(), NA_integer64_), NA_integer64_))
  expect_no_warning(expect_identical(min(integer64(1), integer64(), NA_integer64_, na.rm=TRUE), as.integer64(0L)))
  expect_no_warning(expect_identical(min(integer64(1), integer64()), as.integer64(0L)))
  expect_no_warning(expect_identical(min(integer64(1), integer64(), na.rm=TRUE), as.integer64(0L)))
  # R consistent coercion of min
  expect_no_warning(expect_identical(min(as.integer64(1L), "2"), "1"))
  expect_no_warning(expect_identical(min(as.integer64(1L), 2), 1))
  expect_no_warning(expect_identical(min(as.integer64(1L), c(2, NA), na.rm=TRUE), 1))
  expect_no_warning(expect_identical(min(as.integer64(1L), c(2, NA), na.rm=FALSE), NA_real_))
  expect_warning(expect_identical(min(integer64(), na.rm=TRUE), lim.integer64()[2L]), "no non-NA value")
  expect_warning(expect_identical(min(integer64(), na.rm=FALSE), lim.integer64()[2L]), "no non-NA value")
  expect_warning(expect_identical(min(integer64(), character(), na.rm=TRUE), NA_character_), "no non-missing arguments, returning NA", fixed=TRUE)
  expect_warning(expect_identical(min(integer64(), character(), na.rm=FALSE), NA_character_), "no non-missing arguments, returning NA", fixed=TRUE)
  expect_warning(expect_identical(min(integer64(), numeric(), na.rm=TRUE), Inf), "no non-missing arguments to min; returning Inf", fixed=TRUE)
  expect_warning(expect_identical(min(integer64(), numeric(), na.rm=FALSE), Inf), "no non-missing arguments to min; returning Inf", fixed=TRUE)
  expect_error(min(integer64(), complex(), na.rm=TRUE), "invalid 'type' .*complex.* of argument")
  expect_error(min(integer64(), complex(), na.rm=FALSE), "invalid 'type' .*complex.* of argument")
  expect_warning(expect_identical(min(integer64(), integer(), na.rm=TRUE), lim.integer64()[2L]), "no non-NA value")
  expect_warning(expect_identical(min(integer64(), integer(), na.rm=FALSE), lim.integer64()[2L]), "no non-NA value")
  expect_error(min(10, list()), "invalid 'type' (list) of argument", fixed=TRUE)
  expect_error(min(as.integer64(10L), list()), "invalid 'type' (list) of argument", fixed=TRUE)
  expect_no_warning(expect_identical(min(as.integer64(10L), character()), "10"))
  expect_no_warning(expect_identical(min(as.integer64(10L), NA_character_, na.rm=TRUE), "10"))
  
  expect_identical(max(x), x[10L])
  expect_identical(max(x, as.integer64(11L)), as.integer64(11L))
  expect_warning(expect_identical(max(integer64()), lim.integer64()[1L]), "no non-NA value")
  expect_warning(expect_identical(max(integer64(), na.rm=TRUE), lim.integer64()[1L]), "no non-NA value")
  expect_no_warning(expect_identical(max(x, integer64()), x[10L]))
  expect_no_warning(expect_identical(max(x, integer64(), na.rm=TRUE), x[10L]))
  expect_no_warning(expect_identical(max(lim.integer64()[1L]), lim.integer64()[1L]))
  expect_no_warning(expect_identical(max(lim.integer64()[1L], NA_integer64_, na.rm=TRUE), lim.integer64()[1L]))
  expect_no_warning(expect_identical(max(integer64(), NA_integer64_), NA_integer64_))
  expect_warning(expect_identical(max(integer64(), NA_integer64_, na.rm=TRUE), lim.integer64()[1L]), "no non-NA value")
  expect_no_warning(expect_identical(max(integer64(1), NA_integer64_), NA_integer64_))
  expect_no_warning(expect_identical(max(integer64(1), NA_integer64_, na.rm=TRUE), as.integer64(0L)))
  expect_no_warning(expect_identical(max(integer64(1), integer64(), NA_integer64_), NA_integer64_))
  expect_no_warning(expect_identical(max(integer64(1), integer64(), NA_integer64_, na.rm=TRUE), as.integer64(0L)))
  expect_no_warning(expect_identical(max(integer64(1), integer64()), as.integer64(0L)))
  expect_no_warning(expect_identical(max(integer64(1), integer64(), na.rm=TRUE), as.integer64(0L)))
  # R consistent coercion of max
  expect_no_warning(expect_identical(max(as.integer64(2L), "1"), "2"))
  expect_no_warning(expect_identical(max(as.integer64(2L), 1), 2))
  expect_no_warning(expect_identical(max(as.integer64(2L), c(1, NA), na.rm=TRUE), 2))
  expect_no_warning(expect_identical(max(as.integer64(2L), c(1, NA), na.rm=FALSE), NA_real_))
  expect_warning(expect_identical(max(integer64(), na.rm=TRUE), lim.integer64()[1L]), "no non-NA value")
  expect_warning(expect_identical(max(integer64(), na.rm=FALSE), lim.integer64()[1L]), "no non-NA value")
  expect_warning(expect_identical(max(integer64(), character(), na.rm=TRUE), NA_character_), "no non-missing arguments, returning NA", fixed=TRUE)
  expect_warning(expect_identical(max(integer64(), character(), na.rm=FALSE), NA_character_), "no non-missing arguments, returning NA", fixed=TRUE)
  expect_warning(expect_identical(max(integer64(), numeric(), na.rm=TRUE), -Inf), "no non-missing arguments to max; returning -Inf", fixed=TRUE)
  expect_warning(expect_identical(max(integer64(), numeric(), na.rm=FALSE), -Inf), "no non-missing arguments to max; returning -Inf", fixed=TRUE)
  expect_error(max(integer64(), complex(), na.rm=TRUE), "invalid 'type' .*complex.* of argument")
  expect_error(max(integer64(), complex(), na.rm=FALSE), "invalid 'type' .*complex.* of argument")
  expect_warning(expect_identical(max(integer64(), integer(), na.rm=TRUE), lim.integer64()[1L]), "no non-NA value")
  expect_warning(expect_identical(max(integer64(), integer(), na.rm=FALSE), lim.integer64()[1L]), "no non-NA value")
  expect_error(max(10, list()), "invalid 'type' (list) of argument", fixed=TRUE)
  expect_error(max(as.integer64(10L), list()), "invalid 'type' (list) of argument", fixed=TRUE)
  expect_no_warning(expect_identical(max(as.integer64(10L), character()), "10"))
  expect_no_warning(expect_identical(max(as.integer64(10L), NA_character_, na.rm=TRUE), "10"))

  expect_identical(range(x), x[c(1L, 10L)])
  expect_identical(range(x, x+1L), c(x[1L], x[10L]+1L))
  expect_identical(range(x, NA_integer64_, finite=TRUE), x[c(1L, 10L)])
  expect_warning(expect_identical(range(integer64()), lim.integer64()[2:1]), "no non-NA value")
  expect_warning(expect_identical(range(integer64(), na.rm=TRUE), lim.integer64()[2:1]), "no non-NA value")
  expect_no_warning(expect_identical(range(x, integer64()), x[c(1L, 10L)]))
  expect_no_warning(expect_identical(range(x, integer64(), na.rm=TRUE), x[c(1L, 10L)]))
  expect_no_warning(expect_identical(range(lim.integer64()), lim.integer64()))
  expect_no_warning(expect_identical(range(lim.integer64(), NA_integer64_, na.rm=TRUE), lim.integer64()))
  expect_no_warning(expect_identical(range(integer64(), NA_integer64_), rep(NA_integer64_, 2L)))
  expect_warning(expect_identical(range(integer64(), NA_integer64_, na.rm=TRUE), lim.integer64()[2:1]), "no non-NA value")
  expect_no_warning(expect_identical(range(integer64(1), NA_integer64_), rep(NA_integer64_, 2L)))
  expect_no_warning(expect_identical(range(integer64(1), NA_integer64_, na.rm=TRUE), rep(as.integer64(0L), 2L)))
  expect_no_warning(expect_identical(range(integer64(1), integer64(), NA_integer64_), rep(NA_integer64_, 2L)))
  expect_no_warning(expect_identical(range(integer64(1), integer64(), NA_integer64_, na.rm=TRUE), rep(as.integer64(0L), 2L)))
  expect_no_warning(expect_identical(range(integer64(1), integer64()), rep(as.integer64(0L), 2L)))
  expect_no_warning(expect_identical(range(integer64(1), integer64(), na.rm=TRUE), rep(as.integer64(0L), 2L)))
  # R consistent coercion of range
  expect_no_warning(expect_identical(range(as.integer64(2L), "1"), c("1", "2")))
  expect_no_warning(expect_identical(range(as.integer64(2L), 1), c(1, 2)))
  expect_no_warning(expect_identical(range(as.integer64(2L), c(1, NA), na.rm=TRUE), c(1, 2)))
  expect_no_warning(expect_identical(range(as.integer64(2L), c(1, NA), na.rm=FALSE), rep(NA_real_, 2L)))
  expect_warning(expect_identical(range(integer64(), na.rm=TRUE), lim.integer64()[2:1]), "no non-NA value")
  expect_warning(expect_identical(range(integer64(), na.rm=FALSE), lim.integer64()[2:1]), "no non-NA value")
  expect_warning(
    expect_warning(
      expect_identical(range(integer64(), character(), na.rm=TRUE), rep(NA_character_, 2L)), 
      "no non-missing arguments, returning NA", fixed=TRUE), 
    "no non-missing arguments, returning NA", fixed=TRUE
  )
  expect_warning(
    expect_warning(
      expect_identical(range(integer64(), character(), na.rm=FALSE), rep(NA_character_, 2L)),
      "no non-missing arguments, returning NA", fixed=TRUE), 
    "no non-missing arguments, returning NA", fixed=TRUE
  )
  expect_warning(
    expect_warning(
      expect_identical(range(integer64(), numeric(), na.rm=TRUE), c(Inf, -Inf)), 
      "no non-missing arguments to min; returning Inf", fixed=TRUE), 
    "no non-missing arguments to max; returning -Inf", fixed=TRUE
  )
  expect_warning(
    expect_warning(
      expect_identical(range(integer64(), numeric(), na.rm=FALSE), c(Inf, -Inf)), 
      "no non-missing arguments to min; returning Inf", fixed=TRUE), 
    "no non-missing arguments to max; returning -Inf", fixed=TRUE
  )
  expect_error(range(integer64(), complex(), na.rm=TRUE), "invalid 'type' .*complex.* of argument")
  expect_error(range(integer64(), complex(), na.rm=FALSE), "invalid 'type' .*complex.* of argument")
  expect_warning(expect_identical(range(integer64(), integer(), na.rm=TRUE), lim.integer64()[2:1]), "no non-NA value")
  expect_warning(expect_identical(range(integer64(), integer(), na.rm=FALSE), lim.integer64()[2:1]), "no non-NA value")
  expect_no_warning(expect_identical(range(as.integer64(10:12), list()), as.integer64(c(10L, 12L))))  
  expect_no_warning(expect_identical(range(as.integer64(10:12), list(list())), as.integer64(c(10L, 12L))))  
  expect_no_warning(expect_identical(range(as.integer64(10:12), list(list(character()))), c("10", "12")))  
  expect_no_warning(expect_identical(range(as.integer64(10:12), list(list(1:5), "A")), c("1", "A")))  
  expect_no_warning(expect_identical(range(as.integer64(10:12), character()), c("10", "12")))
  expect_no_warning(expect_identical(range(as.integer64(10:12), NA_character_, na.rm=TRUE), c("10", "12")))
  
  expect_identical(diff(x), as.integer64(rep(1L, 9L)))

  expect_identical(cummin(x), as.integer64(rep(1L, 10L)))
  expect_identical(cummax(x), x)
  expect_identical(cumsum(x), as.integer64(choose(2:11, 2L)))
  expect_identical(cumprod(x), as.integer64(factorial(1:10)))
})

test_that("display methods work", {
  x = as.integer64(1:3)
  expect_identical(format(x), as.character(1:3))
  expect_output(print(x), "integer64.*\\s*1\\s*2\\s*3")
  expect_output(print(x[0L]), "integer64(0)", fixed=TRUE)
  expect_output(str(x), "integer64 [1:3] 1 2 3", fixed=TRUE)
})

test_that("vector builders of integer64 work", {
  x = as.integer64(1:3)
  
  # c.integer64
  expect_identical(c(x, FALSE), as.integer64(c(1:3, 0L)))
  expect_identical(c(x, 4:6), as.integer64(1:6))
  expect_identical(c(x, 4.0, 5.0, 6.0), as.numeric(1:6))
  expect_identical(c(x, as.integer64(4:6)), as.integer64(1:6))

  res32 = list(xx=1L, a=as.integer64(1L), b=list(list(2L)), b="-1")
  res64 = list(xx=as.integer64(1L), a=as.integer64(1L), b=list(list(2L)), b="-1")
  expect_identical(c(xx=as.integer(1L), list(a=as.integer64(1L), b=list(list(2L))), list(b="-1")), res32)
  expect_identical(c(xx=as.integer64(1L), list(a=as.integer64(1L), b=list(list(2L))), list(b="-1")), res64)
  
  res32 = c(xx = "1", a = "1", b = "2", b = "-1")
  res64 = c(xx = "1", a = "1", b = "2", b = "-1")
  expect_identical(c(xx=as.integer(1L), list(a=as.integer(1L), b=list(list(2L))), list(b="-1"), recursive=TRUE), res32)
  expect_identical(c(xx=as.integer64(1L), list(a=as.integer64(1L), b=list(list(2L))), list(b="-1"), recursive=TRUE), res64)
  
  res32 = list(1L, list(a=as.integer64(1L)), b="-1")
  res64 = list(as.integer64(1L), list(a=as.integer64(1L)), b="-1")
  expect_identical(c(as.integer(1L), list(list(a=as.integer64(1L))), list(b="-1")), res32)
  expect_identical(c(as.integer64(1L), list(list(a=as.integer64(1L))), list(b="-1")), res64)

  x32 = matrix(as.integer(1:10), 2)
  x64 = matrix(as.integer64(1:10), 2)
  expect_identical(c(x64, "-1"), c(x32, "-1"))
  expect_identical(c(x64, 1), c(x32, 1))
  expect_identical(c(x64, 1+1i), c(x32, 1+1i))
  expect_identical(c(x64, 1L), as.integer64(c(x32, 1L)))
  expect_identical(c(x64, as.integer64(1L)), as.integer64(c(x32, 1L)))

  # cbind.integer64
  x32 = 1:10
  x64 = as.integer64(x32)
  expect_identical(cbind(x, FALSE), matrix(as.integer64(c(1:3, 0L, 0L, 0L)), nrow=3L, ncol=2L))
  expect_identical(cbind(x, 4:6), matrix(as.integer64(1:6), nrow=3L, ncol=2L))
  expect_identical(cbind(x, 0.0), matrix(as.numeric(c(1:3, 0L, 0L, 0L)), nrow=3L, ncol=2L))
  expect_identical(cbind(x, as.integer64(4:6)), matrix(as.integer64(1:6), nrow=3L, ncol=2L))
  expect_identical(cbind(integer64()), structure(integer64(), dim = 0:1, dimnames = list(NULL, NULL)))
  expect_identical(cbind(NA_integer64_), structure(NA_integer64_, dim = c(1, 1)))
  expect_identical(cbind(x64), structure(x64, dim = c(10, 1)))
  expect_identical(cbind(x64, 45L), structure(as.integer64(c(x32, rep_len(45L, 10))), dim = c(10, 2)))
  expect_identical(cbind(x64, c(TRUE, NA)), structure(as.integer64(c(x32, rep_len(c(TRUE, NA), 10))), dim = c(10, 2)))
  expect_identical(cbind(x64, c(45L, NA)), structure(as.integer64(c(x32, rep_len(c(45L, NA), 10))), dim = c(10, 2)))
  expect_identical(cbind(x64, c(45, NA)), structure(as.numeric(c(x32, rep_len(c(45, NA), 10))), dim = c(10, 2)))
  expect_identical(cbind(x64, c(45+0i, NA)), structure(as.complex(c(x32, rep_len(c(45, NA), 10))), dim = c(10, 2)))
  expect_identical(cbind(x64, c("45", NA)), structure(as.character(c(x32, rep_len(c(45, NA), 10))), dim = c(10, 2)))
  expect_identical(cbind(x64, character()), structure(as.character(x32), dim = c(10, 1)))
  expect_error(cbind(x64, list()), "cbind.integer64 does not support 'type' (list)", fixed=TRUE)
  expect_error(cbind(matrix(x64, 5), list(), NULL, matrix(as.integer(1:10, 2))), "number of rows of matrices must match (see arg 4)", fixed=TRUE)
  expect_error(cbind(matrix(x64, 5), list(), NULL, data.frame(a=10:1, b=LETTERS[1:10])), "arguments imply differing number of rows: 5, 0, 10", fixed=TRUE)
  expect_identical(
    cbind(matrix(x64, 5), data.frame(a=5:1, b=LETTERS[1:5])), 
    data.frame(`1`=x64[1:5], `2`=x64[6:10], a=5:1, b=LETTERS[1:5], check.names=FALSE)
  )
  expect_error(
    cbind(matrix(x64, 5), data.frame(a=9:1, b=LETTERS[1:9])), 
    "arguments imply differing number of rows: 5, 9", fixed=TRUE
  )
  expect_identical(
    cbind(matrix(x64, 5), data.frame(a=10:1, b=LETTERS[1:10])), 
    data.frame(`1`=x64[c(1:5, 1:5)], `2`=x64[c(6:10, 6:10)], a=10:1, b=LETTERS[1:10], check.names=FALSE)
  )
  expect_identical(
    cbind(matrix(x64, 5), data.frame(a=10:1, b=LETTERS[1:10]), yy=as.integer64(-(1:2))), 
    data.frame(`1`=x64[c(1:5, 1:5)], `2`=x64[c(6:10, 6:10)], a=10:1, b=LETTERS[1:10], yy=as.integer64(rep_len(-(1:2), 10)), check.names=FALSE)
  )
  expect_identical(
    cbind(matrix(x64, 5), data.frame(a=as.integer64(10:1), b=LETTERS[1:10]), yy=as.integer64(-(1:2))), 
    data.frame(`1`=x64[c(1:5, 1:5)], `2`=x64[c(6:10, 6:10)], a=as.integer64(10:1), b=LETTERS[1:10], yy=as.integer64(rep_len(-(1:2), 10)), check.names=FALSE)
  )
  
  # rbind.integer64
  expect_identical(rbind(x, FALSE), matrix(as.integer64(c(1:3, 0L, 0L, 0L)), nrow=2L, ncol=3L, byrow=TRUE))
  expect_identical(rbind(x, 4:6), matrix(as.integer64(1:6), nrow=2L, ncol=3L, byrow=TRUE))
  expect_identical(rbind(x, 0.0), matrix(as.numeric(c(1:3, 0L, 0L, 0L)), nrow=2L, ncol=3L, byrow=TRUE))
  expect_identical(rbind(x, as.integer64(4:6)), matrix(as.integer64(1:6), nrow=2L, ncol=3L, byrow=TRUE))

  expect_identical(rbind(integer64()), structure(integer64(), dim = 1:0, dimnames = list(NULL, NULL)))
  expect_identical(rbind(NA_integer64_), structure(NA_integer64_, dim = c(1, 1)))
  expect_identical(rbind(x64), structure(x64, dim = c(1, 10)))
  expect_identical(rbind(x64, 45L), structure(as.integer64({res = rep(x32, each=2); res[c(FALSE, TRUE)] = 45L; res}), dim = c(2, 10)))
  expect_identical(rbind(x64, c(TRUE, NA)), structure(as.integer64({res = rep(x32, each=2); res[c(FALSE, TRUE)] = c(TRUE, NA); res}), dim = c(2, 10)))
  expect_identical(rbind(x64, c(45L, NA)), structure(as.integer64({res = rep(x32, each=2); res[c(FALSE, TRUE)] = c(45L, NA); res}), dim = c(2, 10)))
  expect_identical(rbind(x64, c(45, NA)), structure(as.numeric({res = rep(x32, each=2); res[c(FALSE, TRUE)] = c(45, NA); res}), dim = c(2, 10)))
  expect_identical(rbind(x64, c(45+0i, NA)), structure(as.complex({res = rep(x32, each=2); res[c(FALSE, TRUE)] = c(45+0i, NA); res}), dim = c(2, 10)))
  expect_identical(rbind(x64, c("45", NA)), structure(as.character({res = rep(x32, each=2); res[c(FALSE, TRUE)] = c("45", NA); res}), dim = c(2, 10)))
  expect_identical(rbind(x64, character()), structure(as.character(x32), dim = c(1, 10)))
  expect_error(rbind(x64, list()), "rbind.integer64 does not support 'type' (list)", fixed=TRUE)
  expect_error(rbind(matrix(x64, 5), list(), NULL, matrix(as.integer(1:10, 2))), "number of columns of matrices must match (see arg 4)", fixed=TRUE)
  expect_error(rbind(matrix(x64, 5), list(), NULL, data.frame(a=10:1, b=LETTERS[1:10])), "names do not match previous names", fixed=TRUE)
  expect_identical(
    rbind(matrix(x64, 5, dimnames=list(NULL, c("a", "b"))), list(), NULL, data.frame(a=10:1, b=LETTERS[1:10])), 
    data.frame(a=as.character(c(x32[1:5], 10:1)), b=c(x32[6:10], LETTERS[1:10]), check.names=FALSE)
  )
  expect_identical(
    rbind(matrix(x64, 5, dimnames=list(NULL, c("a", "b"))), list(), NULL, data.frame(a=10:1, b=c(TRUE, FALSE))), 
    data.frame(a=as.integer64(c(x32[1:5], 10:1)), b=as.integer64(c(x32[6:10], rep_len(c(TRUE, FALSE), 10))), check.names=FALSE)
  )
  expect_warning(
    expect_warning(
      expect_identical(
        rbind(matrix(x64, ncol=2), matrix(c(TRUE, FALSE), ncol=2), matrix(10:1, ncol=2), 1:4), 
        structure(as.integer64(rbind(matrix(x32, ncol=2), matrix(c(TRUE, FALSE), ncol=2), matrix(10:1, ncol=2), 1:4)), dim=c(12, 2))
      ), 
      "number of columns of result is not a multiple of vector length (arg 4)", fixed=TRUE), 
    "number of columns of result is not a multiple of vector length (arg 4)", fixed=TRUE
  )

  # rep.integer64
  expect_identical(rep(x, 2L), c(x, x))
  expect_identical(rep(x, each=2L), as.integer64(c(1L, 1L, 2L, 2L, 3L, 3L)))

  expect_identical(x[1L]:x[3L], x)
  expect_identical(x[3L]:x[1L], x[3:1]) # rev() a separate method

  # seq.integer64
  expect_identical(seq(x[1L], x[3L], by=1L), x)
  expect_identical(seq(x[1L], x[3L], by=x[1L]), x)
  expect_identical(seq(x[1L], to=10L, by=1L), as.integer64(1:10))
  expect_identical(seq(x[1L], to=11L, by=2L), as.integer64(c(1L, 3L, 5L, 7L, 9L, 11L)))
  # TODO(#47): More tests when the behavior is corrected.
})

# These tests were previously kept as tests under \examples{\dontshow{...}}.
#   Converted to "proper" unit tests for clarity, after making them more
#   canonical within {testthat}, e.g. better capturing expected warnings,
#   changing stopifnot(identical(...)) to expect_identical(...).
local({
  i <- -999:999
  with_parameters_test_that(
    "Old \\dontshow{} tests in ?format.integer64 continue working",
    {
      r <- as.integer64(round(as.integer(i), s))
      r64 <- round(as.integer64(i), s)
      expect_identical(r, r64)
    },
    s = -3:3,
    .interpret_glue = FALSE
  )
})

test_that("Old \\dontshow{} tests in ?extract.replace.integer64 continue working", {
  r <- c(runif64(1000L, lim.integer64()[1L], lim.integer64()[2L]), NA, -2:2)
  expect_identical(r, as.integer64(as.bitstring(r)))
})

test_that("empty inputs give empty outputs for arithmetic", {
  x = integer64(1L)
  y = integer64(0L)

  expect_identical(x+y, integer64())
  expect_identical(y+x, integer64())

  expect_identical(x-y, integer64())
  expect_identical(y-x, integer64())

  expect_identical(+y, integer64())
  expect_identical(-y, integer64())

  expect_identical(x*y, integer64())
  expect_identical(y*x, integer64())

  expect_identical(x/y, double())
  expect_identical(y/x, double())

  expect_identical(x^y, integer64())
  expect_identical(y^x, integer64())

  expect_identical(x %/% y, integer64())
  expect_identical(y %/% x, integer64())

  expect_identical(x%%y, integer64())
  expect_identical(y%%x, integer64())

  expect_identical(log(x, base=y), double())
  # TODO(#93): don't suppress this warning which is inconsistent with integer()
  expect_identical(suppressWarnings(log(y, base=x)), double())

  expect_identical(x==y, logical())
  expect_identical(y==x, logical())

  expect_identical(x!=y, logical())
  expect_identical(y!=x, logical())

  expect_identical(x>=y, logical())
  expect_identical(y>=x, logical())

  expect_identical(x<=y, logical())
  expect_identical(y<=x, logical())

  expect_identical(x>y, logical())
  expect_identical(y>x, logical())

  expect_identical(x<y, logical())
  expect_identical(y<x, logical())

  expect_identical(x&y, logical())
  expect_identical(y&x, logical())

  expect_identical(x|y, logical())
  expect_identical(y|x, logical())

  expect_identical(xor(x, y), logical())
  expect_identical(xor(y, x), logical())
})

test_that("semantics about mixed types for multiplication are respected", {
  int = 5L
  i64 = as.integer64(2L)
  dbl = 3.5

  # default: "old" semantics, to be deprecated
  expect_identical(i64 * dbl, as.integer64(7L))
  expect_identical(dbl * i64, as.integer64(6L))
  expect_identical(i64 * int, as.integer64(10L))
  expect_identical(int * i64, as.integer64(10L))
  expect_identical(i64 * i64, as.integer64(4L))

  skip_if_not_installed("withr") # only really for testing without testthat
  withr::with_options(list(integer64_semantics = "new"), {
    expect_identical(i64 * dbl, as.integer64(7L))
    expect_identical(dbl * i64, as.integer64(7L))
    expect_identical(i64 * int, as.integer64(10L))
    expect_identical(int * i64, as.integer64(10L))
    expect_identical(i64 * i64, as.integer64(4L))
  })
})

test_that("semantics about mixed types for division are respected", {
  int = 10L
  i64 = as.integer64(5L)
  dbl = 2.5

  # default: "old" semantics, to be deprecated
  expect_identical(i64 / dbl, 2.0)
  expect_identical(dbl / i64, 0.4)
  expect_identical(i64 / int, 0.5)
  expect_identical(int / i64, 2.0)
  expect_identical(i64 / i64, 1.0)

  skip_if_not_installed("withr") # only really for testing without testthat
  withr::with_options(list(integer64_semantics = "new"), {
    expect_identical(i64 / dbl, 2.0)
    expect_identical(dbl / i64, 0.5)
    expect_identical(i64 / int, 0.5)
    expect_identical(int / i64, 2.0)
    expect_identical(i64 / i64, 1.0)
  })
})

test_that("all.equal.integer64 reflects changes for vector scale= from all.equal.numeric", {
  # same test as for base R, multiplied by 1000 so the inputs are all integer64
  expect_identical(
    all.equal(
      as.integer64(c(1000L, 1000L)),
      as.integer64(c(1010L, 1010L)),
      scale = c(10.0, 10.0)
    ),
    "Mean scaled difference: 1"
  )
  # same test as for base R, multiplied by 1e9
  one_e9 = as.integer64(1000000000L)
  expect_true(all.equal(
    rep(one_e9, 5L),
    one_e9 + (-1L:3), # TODO(r-lib/lintr#): no 'L'
    scale = (1:5)*one_e9
  ))
})

test_that("all.equal works", {
  x = y = as.integer64(1L)

  expect_true(all.equal(x, x))

  class(y) = c("xx", "integer64")
  expect_match(all.equal(x, y), "target is integer64, current is xx", fixed=TRUE, all=FALSE)
  expect_match(all.equal(x[0L], x[1L]), "integer64: lengths.*differ", all=FALSE)

  class(y) = "integer64"
  attr(y, "xx") = "zz"
  expect_match(all.equal(x, y), "Attributes", fixed=TRUE)
  expect_no_match(
    expect_match(all.equal(x[0L], y), "integer64: lengths.*differ", all=FALSE),
    "Lengths:", fixed = TRUE
  )

  y = NA_integer64_
  expect_match(all.equal(x, y), "'is.NA' value mismatch", fixed=TRUE)

  x = as.integer64(1000000000L)
  expect_true(all.equal(x, x+1L))
  expect_true(all.equal(x, x+1L, tolerance=1.0e9)) # forcing scale=1
  expect_match(all.equal(x, x+100L), "Mean relative difference", fixed=TRUE)
  expect_match(all.equal(x, x+1L, scale=1.0), "Mean absolute difference", fixed=TRUE)
})


test_that("union works", {
  
  # identical to base for basic data types
  expect_identical(union(NULL, 5:10), base::union(NULL, 5:10))
  expect_identical(union(numeric(), 5:10), base::union(numeric(), 5:10))
  expect_identical(union(logical(), 5:10), base::union(logical(), 5:10))
  expect_identical(union(1.5:7, 5:10), base::union(1.5:7, 5:10))
  expect_identical(union(1:7, NULL), base::union(1:7, NULL))
  expect_identical(union(c(5, 3, 5), c(0, 2, 1, 3, 6)), base::union(c(5, 3, 5), c(0, 2, 1, 3, 6)))
  expect_identical(union("a", 1L), base::union("a", 1L))
  expect_identical(union(NA, NA_integer_), base::union(NA, NA_integer_))
  expect_identical(union(NA_integer_, NA), base::union(NA_integer_, NA))
  
  expect_identical(union(NULL, as.integer64(5:10)), as.integer64(5:10))
  expect_identical(union(numeric(), as.integer64(5:10)), as.numeric(5:10))
  expect_identical(union(logical(), as.integer64(5:10)), as.integer64(5:10))
  expect_identical(union(1.5:7, as.integer64(5:10)), c(seq(1.5, 6.5), 5:10))
  expect_identical(union(as.integer64(1:7), NULL), as.integer64(1:7))
  expect_identical(union(c(5L, 3L, 5L), as.integer64(c(0, 2, 1, 3, 6))), as.integer64(c(5, 3, 0, 2, 1, 6)))
  expect_identical(union("a", as.integer64(1L)), c("a", "1"))
  expect_identical(union(NA, NA_integer64_), NA_integer64_)
  expect_identical(union(NA_integer64_, NA), NA_integer64_)
  
})

test_that("intersect works", {
  
  # identical to base for basic data types
  expect_identical(intersect(NULL, 5:10), base::intersect(NULL, 5:10))
  expect_identical(intersect(numeric(), 5:10), base::intersect(numeric(), 5:10))
  expect_identical(intersect(logical(), 5:10), base::intersect(logical(), 5:10))
  expect_identical(intersect(1.5:7, 5:10), base::intersect(1.5:7, 5:10))
  expect_identical(intersect(1:7, NULL), base::intersect(1:7, NULL))
  expect_identical(intersect(c(5, 3, 5), c(0, 2, 1, 3, 6)), base::intersect(c(5, 3, 5), c(0, 2, 1, 3, 6)))
  expect_identical(intersect(1, 1L), base::intersect(1, 1L))
  expect_identical(intersect(1L, 1), base::intersect(1L, 1))
  expect_identical(intersect(c("a", "1"), 1:2), base::intersect(c("a", "1"), 1:2))
  expect_identical(intersect(NA, NA_integer_), base::intersect(NA, NA_integer_))
  expect_identical(intersect(NA_integer_, NA), base::intersect(NA_integer_, NA))
  
  expect_identical(intersect(NULL, as.integer64(5:10)), NULL)
  expect_identical(intersect(numeric(), as.integer64(5:10)), numeric())
  expect_identical(intersect(logical(), as.integer64(5:10)), integer64())
  expect_identical(intersect(1.5:7, as.integer64(5:10)), numeric())
  expect_identical(intersect(as.integer64(1:7), NULL), NULL)
  expect_identical(intersect(as.integer64(c(5, 3, 5)), as.integer64(c(0, 2, 1, 3, 6))), as.integer64(3L))
  expect_identical(intersect(1, as.integer64(1L)), 1)
  expect_identical(intersect(as.integer64(1L), 1), 1)
  expect_identical(intersect(1L, as.integer64(1L)), as.integer64(1L))
  expect_identical(intersect(as.integer64(1L), 1L), as.integer64(1L))
  expect_identical(intersect(c("a", "1"), as.integer64(1:2)), "1")
  expect_identical(intersect(NA, NA_integer64_), NA_integer64_)
  expect_identical(intersect(NA_integer64_, NA), NA_integer64_)

})

test_that("setdiff works", {
  
  expect_identical(setdiff(NULL, 5:10), base::setdiff(NULL, 5:10))
  expect_identical(setdiff(numeric(), 5:10), base::setdiff(numeric(), 5:10))
  expect_identical(setdiff(logical(), 5:10), base::setdiff(logical(), 5:10))
  expect_identical(setdiff(1.5:7, 5:10), base::setdiff(1.5:7, 5:10))
  expect_identical(setdiff(1:7, NULL), base::setdiff(1:7, NULL))
  expect_identical(setdiff(c(5, 3, 5), c(0, 2, 1, 3, 6)), base::setdiff(c(5, 3, 5), c(0, 2, 1, 3, 6)))
  expect_identical(setdiff(c(1, 2), 1L), base::setdiff(c(1, 2), 1L))
  expect_identical(setdiff(1:2, 1), base::setdiff(1:2, 1))
  expect_identical(setdiff(c("a", "1"), 1:2), base::setdiff(c("a", "1"), 1:2))
  expect_identical(setdiff(NA, NA_integer_), base::setdiff(NA, NA_integer_))
  expect_identical(setdiff(NA_integer_, NA), base::setdiff(NA_integer_, NA))
  
  expect_identical(setdiff(NULL, as.integer64(5:10)), NULL)
  expect_identical(setdiff(numeric(), as.integer64(5:10)), numeric())
  expect_identical(setdiff(logical(), as.integer64(5:10)), logical())
  expect_identical(setdiff(1.5:7, as.integer64(5:10)), seq(1.5, 6.5))
  expect_identical(setdiff(as.integer64(1:7), NULL), as.integer64(1:7))
  expect_identical(setdiff(as.integer64(c(5, 3, 5)), as.integer64(c(0, 2, 1, 3, 6))), as.integer64(5L))
  expect_identical(setdiff(1:2, as.integer64(1L)), 2L)
  expect_identical(setdiff(c(1, 2), as.integer64(1L)), 2)
  expect_identical(setdiff(as.integer64(1:2), 1), as.integer64(2L))
  expect_identical(setdiff(1L, as.integer64(1L)), integer())
  expect_identical(setdiff(as.integer64(1L), 1L), integer64())
  expect_identical(setdiff(c("a", "1"), as.integer64(1:2)), "a")
  expect_identical(setdiff(NA, NA_integer64_), logical())
  expect_identical(setdiff(NA_integer64_, NA), integer64())
  
})

test_that("setequal works", {
  
  expect_identical(setequal(NULL, 5:10), base::setequal(NULL, 5:10))
  expect_identical(setequal(numeric(), 5:10), base::setequal(numeric(), 5:10))
  expect_identical(setequal(logical(), 5:10), base::setequal(logical(), 5:10))
  expect_identical(setequal(1.5:7, 5:10), base::setequal(1.5:7, 5:10))
  expect_identical(setequal(1:7, NULL), base::setequal(1:7, NULL))
  expect_identical(setequal(c(5, 3, 5), c(3L, 5L)), base::setequal(c(5, 3, 5), c(3L, 5L)))
  expect_identical(setequal(c(1, 2), 1L), base::setequal(c(1, 2), 1L))
  expect_identical(setequal(1:2, 1), base::setequal(1:2, 1))
  expect_identical(setequal(c("a", "1"), 1:2), base::setequal(c("a", "1"), 1:2))
  expect_identical(setequal(NA, NA_integer_), base::setequal(NA, NA_integer_))
  expect_identical(setequal(NA_integer_, NA), base::setequal(NA_integer_, NA))
  
  expect_identical(setequal(NULL, as.integer64(5:10)), FALSE)
  expect_identical(setequal(numeric(), as.integer64(5:10)), FALSE)
  expect_identical(setequal(logical(), as.integer64(5:10)), FALSE)
  expect_identical(setequal(1.5:7, as.integer64(5:10)), FALSE)
  expect_identical(setequal(as.integer64(1:7), NULL), FALSE)
  expect_identical(setequal(as.integer64(c(5, 3, 5)), as.integer64(c(3, 5))), TRUE)
  expect_identical(setequal(1:2, as.integer64(1L)), FALSE)
  expect_identical(setequal(as.integer64(1L), 1:2), FALSE)
  expect_identical(setequal(1L, as.integer64(1L)), TRUE)
  expect_identical(setequal(as.integer64(1L), 1L), TRUE)
  expect_identical(setequal(c("a", "1"), as.integer64(1:2)), FALSE)
  expect_identical(setequal(NA, NA_integer64_), TRUE)
  expect_identical(setequal(NA_integer64_, NA), TRUE)
  
})


test_that("extraction and replacement works consistent to integer", {

  # extraction with `[`
  x = as.integer(1:10)
  names(x) = letters[seq_along(x)]
  y = as.integer64(x)
  names(y) = letters[seq_along(y)]
  sel = c(TRUE, FALSE, NA, TRUE)
  expect_identical(y[sel], as.integer64(x[sel], keep.names=TRUE))
  sel = c(1, NA, 3, 11)
  expect_identical(y[sel], as.integer64(x[sel], keep.names=TRUE))
  expect_identical(y[as.integer64(sel)], as.integer64(x[sel], keep.names=TRUE))
  sel = c(-1, -3, 0, -11)
  expect_identical(y[sel], as.integer64(x[sel], keep.names=TRUE))
  sel = c(-1, -3, 0, -11, NA)
  expect_error(x[sel], "only 0's may be mixed with negative subscripts", fixed=TRUE)
  expect_error(y[sel], "only 0's may be mixed with negative subscripts", fixed=TRUE)

  expect_identical(as.integer64(c("9218868437227407266", "1"))[c(1,NA,3,4)], structure(as.integer64(c("9218868437227407266", NA_character_, NA_character_, NA_character_))))

  sel = c("d", "", "b", NA_character_)
  expect_identical(y[sel], as.integer64(x[sel], keep.names=TRUE))

  m32 = matrix(1:10, nrow=2L)
  m64 = matrix(as.integer64(m32), nrow=dim(m32)[1L])
  expect_identical(m32[integer(), 1:2, drop=TRUE], structure(integer(), dim = c(0L, 2L)))
  expect_identical(m64[integer(), 1:2, drop=TRUE], structure(integer64(), dim = c(0L, 2L)))

  expect_identical(m32[1:2, integer(), drop=TRUE], structure(integer(), dim = c(2L, 0L)))
  expect_identical(m64[1:2, integer(), drop=TRUE], structure(integer64(), dim = c(2L, 0L)))

  expect_identical(m32[integer(), 1:2, drop=FALSE], structure(integer(), dim = c(0L, 2L)))
  expect_identical(m64[integer(), 1:2, drop=FALSE], structure(integer64(), dim = c(0L, 2L)))

  expect_identical(m32[1:2, integer(), drop=FALSE], structure(integer(), dim = c(2L, 0L)))
  expect_identical(m64[1:2, integer(), drop=FALSE], structure(integer64(), dim = c(2L, 0L)))

  expect_identical(m32[1:2, 1:3, drop=TRUE], structure(as.integer(1:6), dim = c(2L, 3L)))
  expect_identical(m64[1:2, 1:3, drop=TRUE], structure(as.integer64(1:6), dim = c(2L, 3L)))

  expect_identical(m32[1:2], structure(as.integer(1:2)))
  expect_identical(m64[1:2], structure(as.integer64(1:2)))

  expect_identical(m32[1:2, drop=TRUE], structure(as.integer(1:2)))
  expect_identical(m64[1:2, drop=TRUE], structure(as.integer64(1:2)))

  expect_identical(m32[j = 1:3, drop=TRUE], structure(as.integer(1:3)))
  expect_identical(m64[j = 1:3, drop=TRUE], structure(as.integer64(1:3)))

  expect_identical(m32[1:2, , drop=TRUE], structure(as.integer(1:10), dim = c(2L, 5L)))
  expect_identical(m64[1:2, , drop=TRUE], structure(as.integer64(1:10), dim = c(2L, 5L)))

  expect_identical(m32[, 1:3, drop=TRUE], structure(as.integer(1:6), dim = c(2L, 3L)))
  expect_identical(m64[, 1:3, drop=TRUE], structure(as.integer64(1:6), dim = c(2L, 3L)))

  expect_identical(m32[1, , drop=TRUE], structure(as.integer(c(1L, 3L, 5L, 7L, 9L))))
  expect_identical(m64[1, , drop=TRUE], structure(as.integer64(c(1L, 3L, 5L, 7L, 9L))))

  expect_identical(m32[1, , drop=FALSE], structure(as.integer(c(1L, 3L, 5L, 7L, 9L)), dim = c(1L, 5L)))
  expect_identical(m64[1, , drop=FALSE], structure(as.integer64(c(1L, 3L, 5L, 7L, 9L)), dim = c(1L, 5L)))

  expect_identical(m32[, 1, drop=TRUE], structure(as.integer(1:2)))
  expect_identical(m64[, 1, drop=TRUE], structure(as.integer64(1:2)))

  expect_identical(m32[, 1, drop=FALSE], structure(as.integer(1:2), dim = 2:1))
  expect_identical(m64[, 1, drop=FALSE], structure(as.integer64(1:2), dim = 2:1))

  expect_identical(m32[c(9, NA, 11, 12), drop=FALSE], structure(as.integer(c(9L, NA, NA, NA))))
  expect_identical(m64[c(9, NA, 11, 12), drop=FALSE], structure(as.integer64(c(9L, NA, NA, NA))))

  expect_identical(m32[integer(), c(1:2, 0, NA), drop=TRUE], structure(integer(), dim = c(0L, 3L)))
  expect_identical(m64[integer(), c(1:2, 0, NA), drop=TRUE], structure(integer64(), dim = c(0L, 3L)))
  expect_identical(m64[integer64(), c(1:2, 0, NA), drop=TRUE], structure(integer64(), dim = c(0L, 3L)))

  expect_identical(m32[, c(1:2, 0, NA), drop=TRUE], structure(as.integer(c(1:4, NA, NA)), dim = c(2L, 3L)))
  expect_identical(m64[, c(1:2, 0, NA), drop=TRUE], structure(as.integer64(c(1:4, NA, NA)), dim = c(2L, 3L)))

  expect_identical(m32[c(1, NA, 2), 1:3, drop=TRUE], structure(as.integer(c(1L, NA, 2L, 3L, NA, 4L, 5L, NA, 6L)), dim = c(3L, 3L)))
  expect_identical(m64[c(1, NA, 2), 1:3, drop=TRUE], structure(as.integer64(c(1L, NA, 2L, 3L, NA, 4L, 5L, NA, 6L)), dim = c(3L, 3L)))

  m32 = matrix(1:10, 2L, dimnames = list(LETTERS[1:2], letters[1:5]))
  m64 = matrix(as.integer64(1:10), 2L, dimnames = list(LETTERS[1:2], letters[1:5]))

  expect_error(m32[c("B", "D", "A"), c("d", "a")], "subscript out of bounds", fixed=TRUE)
  expect_error(m64[c("B", "D", "A"), c("d", "a")], "subscript out of bounds", fixed=TRUE)
  
  expect_identical(m32[c("B", "D", "A")], rep(NA_integer_, 3L))
  expect_identical(m64[c("B", "D", "A")], rep(NA_integer64_, 3L))
  
  a32 = array(as.integer(1:27), c(3,3,3))
  a64 = array(as.integer64(1:27), c(3,3,3))
  
  expect_identical(a32[2, , 3, drop=FALSE], structure(as.integer(c(20L, 23L, 26L)), dim = c(1L, 3L, 1L)))
  expect_identical(a64[2, , 3, drop=FALSE], structure(as.integer64(c(20L, 23L, 26L)), dim = c(1L, 3L, 1L)))

  expect_identical(a32[2, , 3, drop=TRUE], structure(as.integer(c(20L, 23L, 26L))))
  expect_identical(a64[2, , 3, drop=TRUE], structure(as.integer64(c(20L, 23L, 26L))))

  expect_identical(a32[1, c(1, 3, 2), 2:3, drop=TRUE], structure(as.integer(c(10L, 16L, 13L, 19L, 25L, 22L)), dim = 3:2))
  expect_identical(a64[1, c(1, 3, 2), 2:3, drop=TRUE], structure(as.integer64(c(10L, 16L, 13L, 19L, 25L, 22L)), dim = 3:2))

  expect_identical(a32[, c(1, 2, 0, 3, NA, 1), c(TRUE, FALSE, NA), drop=FALSE], structure(as.integer(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, NA, NA, NA, 1L, 2L, 3L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)), dim = c(3L, 5L, 2L)))
  expect_identical(a64[, c(1, 2, 0, 3, NA, 1), c(TRUE, FALSE, NA), drop=FALSE], structure(as.integer64(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, NA, NA, NA, 1L, 2L, 3L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)), dim = c(3L, 5L, 2L)))

  expect_identical(a32[, c(1, 2, 0, 3, NA, 1), c(TRUE, FALSE, NA), drop=TRUE], structure(as.integer(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, NA, NA, NA, 1L, 2L, 3L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)), dim = c(3L, 5L, 2L)))
  expect_identical(a64[, c(1, 2, 0, 3, NA, 1), c(TRUE, FALSE, NA), drop=TRUE], structure(as.integer64(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, NA, NA, NA, 1L, 2L, 3L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)), dim = c(3L, 5L, 2L)))

  expect_identical(a32[c(1, 0, 7, NA, 27, 28), drop=FALSE], structure(as.integer(c(1L, 7L, NA, 27L, NA))))
  expect_identical(a64[c(1, 0, 7, NA, 27, 28), drop=FALSE], structure(as.integer64(c(1L, 7L, NA, 27L, NA))))

  expect_identical(a32[c(TRUE, FALSE, NA, TRUE), drop=FALSE], structure(as.integer(c(1L, NA, 4L, 5L, NA, 8L, 9L, NA, 12L, 13L, NA, 16L, 17L, NA, 20L, 21L, NA, 24L, 25L, NA))))
  expect_identical(a64[c(TRUE, FALSE, NA, TRUE), drop=FALSE], structure(as.integer64(c(1L, NA, 4L, 5L, NA, 8L, 9L, NA, 12L, 13L, NA, 16L, 17L, NA, 20L, 21L, NA, 24L, 25L, NA))))

  expect_identical(a32[-1, , -c(0, 2:3), drop=FALSE], structure(as.integer(c(2L, 3L, 5L, 6L, 8L, 9L)), dim = c(2L, 3L, 1L)))
  expect_identical(a64[-1, , -c(0, 2:3), drop=FALSE], structure(as.integer64(c(2L, 3L, 5L, 6L, 8L, 9L)), dim = c(2L, 3L, 1L)))

  expect_identical(a32[-1, 2, -c(0, 2:3), drop=FALSE], structure(as.integer(5:6), dim = c(2L, 1L, 1L)))
  expect_identical(a64[-1, 2, -c(0, 2:3), drop=FALSE], structure(as.integer64(5:6), dim = c(2L, 1L, 1L)))

  expect_identical(a32[-1, 2, -c(0, 2:3), drop=TRUE], structure(as.integer(5:6)))
  expect_identical(a64[-1, 2, -c(0, 2:3), drop=TRUE], structure(as.integer64(5:6)))

  
  # replacement with `[<-`
  x = as.integer(1:10)
  names(x) = letters[seq_along(x)]
  y = as.integer64(x)
  names(y) = letters[seq_along(y)]

  sel = c("d", "", "b", NA_character_)
  x[sel] = 100L
  y[sel] = 100L
  expect_identical(y, structure(as.integer64(x), names = names(x)))
  
  m32 = matrix(1:10, 2L, dimnames = list(LETTERS[1:2], letters[1:5]))
  m64 = matrix(as.integer64(1:10), 2L, dimnames = list(LETTERS[1:2], letters[1:5]))

  m32[1, c(1, 3, NA)] = 100L
  m64[1, c(1, 3, NA)] = as.integer64(100L)
  expect_identical(m64, structure(as.integer64(m32), dim = dim(m32), dimnames = dimnames(m32)))
  
  m32[1, c(1, 4, NA)] = 101L
  m64[1, c(1, 4, NA)] = 101L
  expect_identical(m64, structure(as.integer64(m32), dim = dim(m32), dimnames = dimnames(m32)))
  
  m32[1, c(1, 5, NA)] = 102
  m64[1, c(1, 5, NA)] = 102
  expect_identical(m64, m32)
  
  m32 = matrix(1:10, 2L, dimnames = list(LETTERS[1:2], letters[1:5]))
  m64 = matrix(as.integer64(1:10), 2L, dimnames = list(LETTERS[1:2], letters[1:5]))
  m32[1, c(1, 3, NA)] = "103"
  m64[1, c(1, 3, NA)] = "103"
  expect_identical(m64, m32)

  m32 = matrix(1:10, 2L, dimnames = list(LETTERS[1:2], letters[1:5]))
  m64 = matrix(as.integer64(1:10), 2L, dimnames = list(LETTERS[1:2], letters[1:5]))
  m32[1, c(1, 3, NA)] = 101L
  m64[1, as.integer64(c(1, 3, NA))] = 101L
  expect_identical(m64, structure(as.integer64(m32), dim = dim(m32), dimnames = dimnames(m32)))
  
  m32[, -(1:3)] = 102L
  m64[, -(1:3)] = 102L
  expect_identical(m64, structure(as.integer64(m32), dim = dim(m32), dimnames = dimnames(m32)))
  
  
  # extraction with `[[`  
  x = as.integer(1:10)
  names(x) = letters[seq_along(x)]
  y = as.integer64(x)
  names(y) = letters[seq_along(y)]
  expect_identical(y[[3]], as.integer64(x[[3]]))
  expect_identical(y[["d"]], as.integer64(x[["d"]]))
  
  m32 = matrix(1:10, 2L, dimnames = list(LETTERS[1:2], letters[1:5]))
  m64 = matrix(as.integer64(1:10), 2L, dimnames = list(LETTERS[1:2], letters[1:5]))
  expect_identical(m64[[1, 2]], as.integer64(m32[[1, 2]]))
  expect_identical(m64[[as.integer64(1L), as.integer64(2L)]], as.integer64(m32[[1, 2]]))
  expect_identical(m64[["A", "d"]], as.integer64(m32[["A", "d"]]))

  expect_identical(m64[[1]], as.integer64(m32[[1]]))
  expect_identical(m64[[as.integer64(1L)]], as.integer64(m32[[1]]))

  expect_error(m32[[NA]], "subscript out of bounds", fixed=TRUE)
  expect_error(m64[[NA]], "subscript out of bounds", fixed=TRUE)
  expect_error(m64[[as.integer64(NA)]], "subscript out of bounds", fixed=TRUE)

  expect_error(m32[[0L]], "attempt to select less than one element in integerOneIndex", fixed=TRUE)
  expect_error(m64[[0L]], "attempt to select less than one element in integerOneIndex", fixed=TRUE)
  expect_error(m64[[as.integer64(0L)]], "attempt to select less than one element in integerOneIndex", fixed=TRUE)

  expect_error(m32[[integer()]], "attempt to select less than one element in get1index", fixed=TRUE)
  expect_error(m64[[integer()]], "attempt to select less than one element in get1index", fixed=TRUE)
  expect_error(m64[[as.integer64()]], "attempt to select less than one element in get1index", fixed=TRUE)

  
  # replacement with `[[<-`
  x[["e"]] = 100L
  y[["e"]] = 100L
  expect_identical(y, structure(as.integer64(x), names = names(x)))

  m32 = matrix(1:10, 2L, dimnames = list(LETTERS[1:2], letters[1:5]))
  m64 = matrix(as.integer64(1:10), 2L, dimnames = list(LETTERS[1:2], letters[1:5]))

  m32[[1, 3]] = 110L
  m64[[1, 3]] = 110L
  expect_identical(m64, structure(as.integer64(m32), dim = dim(m32), dimnames = dimnames(m32)))

  m32[["A", "e"]] = 112L
  m64[["A", "e"]] = 112L
  expect_identical(m64, structure(as.integer64(m32), dim = dim(m32), dimnames = dimnames(m32)))

  m32[[1, 4]] = "111"
  m64[[1, 4]] = "111"
  expect_identical(m64, m32)

})


test_that("allNA and anyNA", {

  # Hopefully allNA is added to the base package like anyNA is.
  # expect_no_warning(expect_identical(allNA(as.integer64(c(1L, 1L))), FALSE))
  # expect_no_warning(expect_identical(allNA(as.integer64(c(1L, NA))), FALSE))
  # expect_no_warning(expect_identical(allNA(as.integer64(c(NA, NA))), TRUE))
  # expect_no_warning(expect_identical(allNA(integer64()), FALSE))
  # test the `allNA` function replacement
  allNA = function(x) .Call(bit64.dev:::C_r_ram_integer64_all_na, x=x)
  expect_no_warning(expect_identical(allNA(as.integer64(c(1L, 1L))), FALSE))
  expect_no_warning(expect_identical(allNA(as.integer64(c(1L, NA))), FALSE))
  expect_no_warning(expect_identical(allNA(as.integer64(c(NA, NA))), TRUE))
  expect_no_warning(expect_identical(allNA(integer64()), TRUE))

  expect_identical(anyNA(as.integer64(c(1L, 1L))), anyNA(c(1L, 1L)))
  expect_identical(anyNA(as.integer64(c(1L, NA))), anyNA(c(1L, NA)))
  expect_identical(anyNA(as.integer64(c(NA, NA))), anyNA(c(NA, NA)))
  expect_identical(anyNA(integer64()), anyNA(integer()))

})
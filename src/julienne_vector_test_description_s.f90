submodule(julienne_vector_test_description_m) julienne_vector_test_description_s
  use assert_m, only : assert
  implicit none

contains

  module procedure contains_text
    integer i
    associate(num_descriptions => size(self%description_vector_))
      allocate(match_vector(num_descriptions))
      do i = 1, num_descriptions
        match_vector(i) = index(self%description_vector_(i)%string(), substring ) /= 0
      end do
    end associate
  end procedure

  module procedure construct
    vector_test_description%description_vector_ = description_vector
    vector_test_description%vector_function_strategy_ = vector_function_strategy
  end procedure

  module procedure run
    associate(vector_result => self%vector_function_strategy_%vector_function())
      call assert(size(self%description_vector_)==size(vector_result), "julienne_vector_test_description_s: size match")
      test_results = test_result_t(self%description_vector_, vector_result)
    end associate
  end procedure

end submodule julienne_vector_test_description_s

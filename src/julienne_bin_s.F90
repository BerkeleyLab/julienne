! Copyright (c) 2024-2025, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(julienne_bin_m) julienne_bin_s
  use julienne_assert_m, only : call_julienne_assert_
  use julienne_test_diagnosis_m, only : operator(.isAtLeast.)
  implicit none

contains

  module procedure construct

      call_julienne_assert(num_items .isAtLeast. num_bins)

      associate( remainder => mod(num_items, num_bins), items_per_bin => num_items/num_bins)

        if (bin_number <= remainder) then
          bin%first_ = 1 + (bin_number-1)*(items_per_bin+1)
          bin%last_  = bin_number*(items_per_bin+1)
        else
          bin%first_ = 1 + (remainder-1)*(items_per_bin+1) + 1 + (bin_number-remainder)*items_per_bin
          bin%last_ = remainder*(items_per_bin+1) + (bin_number-remainder)*items_per_bin
        end if

      end associate

  end procedure

  module procedure first
    first_item_number  = self%first_
  end procedure

  module procedure last
    last_item_number = self%last_
  end procedure

end submodule julienne_bin_s

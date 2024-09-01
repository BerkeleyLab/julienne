submodule(julienne_bin_m) julienne_bin_s
  use assert_m, only : assert, intrinsic_array_t
  implicit none

contains

  module procedure construct

      call assert( num_items>=num_bins, "bin_s(construct): num_items>=num_bins", intrinsic_array_t([num_items,num_bins]))

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

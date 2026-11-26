module radiation_sizes
 use config,only: max_levels
 implicit none
 ! Capacity of the radiation layers. LW adds max(nint(p_top/4 hPa), 1) layers above the model top (radiation_lw.f90, deltap),
 ! SW adds one; a run whose LW layers exceed max_lw_layers stops with "LW layer capacity exceeded". Each column passes its
 ! own active layer count.
 integer, parameter :: max_lw_layers=max_levels+4, max_sw_layers=max_levels+1
end module

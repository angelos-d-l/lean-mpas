! vinterp_ozn of physics/mpas_atmphys_o3climatology.F for one column: the climatology's 59 levels to the radiation layers.
module radiation_ozone
 implicit none
 private
 public :: ozone_to_layers
contains
 subroutine ozone_to_layers(nlayers,pressure,pin,ozmix,ozone)
 ! Original vinterp_ozn, one column. Visit layers from top to bottom;
 ! pressure and output remain in the state order, without reversal buffers.
 integer, intent(in) :: nlayers
 real, intent(in) :: pressure(:),pin(59),ozmix(59)
 real, intent(out) :: ozone(:)
 integer :: k,kk,kupper,kkstart,kount,layer
 real :: dpu,dpl
 kupper=1
 do k=1,nlayers
  layer=nlayers-k+1
  kkstart=min0(59,kupper)
  kount=0
  do kk=kkstart,58
   if(pin(kk)<pressure(layer) .and. pressure(layer)<=pin(kk+1)) then
    kupper=kk
    kount=kount+1
   endif
   if(kount==1) then
    dpu=pressure(layer)-pin(kupper)
    dpl=pin(kupper+1)-pressure(layer)
    ozone(layer)=(ozmix(kupper)*dpl+ozmix(kupper+1)*dpu)/(dpl+dpu)
    goto 35
   endif
  enddo
  if(pressure(layer)<pin(1)) then
   ozone(layer)=ozmix(1)*pressure(layer)/pin(1)
  elseif(pressure(layer)>pin(59)) then
   ozone(layer)=ozmix(59)
  else
   dpu=pressure(layer)-pin(kupper)
   dpl=pin(kupper+1)-pressure(layer)
   ozone(layer)=(ozmix(kupper)*dpl+ozmix(kupper+1)*dpu)/(dpl+dpu)
  endif
35 continue
 enddo
 end subroutine
end module

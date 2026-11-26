! lean_xml on one streams file (tests/streams/run.py):  xml_main FILE
! Prints the syntax messages, then the element tree (depth, name, attributes), for comparison with MPAS's parser.
program xml_main
 use lean_xml
 use lean_messages
 implicit none
 type(xml_doc) :: doc
 type(message_list) :: msgs
 character(len=512) :: path
 integer :: i, j, p, depth
 logical :: ok
 call get_command_argument(1, path)
 call xml_load(trim(path), doc)
 ok = xml_syntax_ok(doc, msgs)
 call write_messages(6, msgs)
 if (.not. ok) then
  write(6, '(a)') '@ syntax failed'
 else
  write(6, '(a)') '@ syntax ok'
  if (.not. doc%tree_ok) then
   write(6, '(a)') '@ tree failed: '//doc%tree_error
  else
   do i = 1, doc%n
    depth = 0
    p = doc%el(i)%parent
    do while (p /= 0)
     depth = depth + 1
     p = doc%el(p)%parent
    end do
    write(6, '(a,i0,a)', advance='no') '@ ', depth, ' '//doc%el(i)%name
    do j = 1, size(doc%el(i)%attr)
     write(6, '(a)', advance='no') ' '//doc%el(i)%attr(j)%name//'="'//doc%el(i)%attr(j)%value//'"'
    end do
    write(6, '(a)') ''
   end do
  end if
 end if
end program

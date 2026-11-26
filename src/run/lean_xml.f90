! MPAS syntax passes: framework/xml_stream_parser.c:156-269,596-775.
! Attribute/tree rules: external/ezxml/ezxml.c:67-97,165-224,477-607.
module lean_xml
 use lean_messages, only: message_list, add_message, int_text, LOG_ERR
 implicit none
 private
 public :: xml_attribute, xml_element, xml_doc, xml_load, xml_syntax_ok, xml_attr, xml_children
 type xml_attribute
  character(len=:), allocatable :: name, value
 end type
 type xml_element
  character(len=:), allocatable :: name
  integer :: parent = 0
  type(xml_attribute), allocatable :: attr(:)
 end type
 type xml_doc
  character(len=:), allocatable :: path, text, tree_error
  logical :: read_ok = .false., tree_ok = .false.
  type(xml_element), allocatable :: el(:)
  integer :: n = 0
 end type
contains
 character function ch(text, i)
  character(len=*), intent(in) :: text
  integer, intent(in) :: i
  ch = achar(0)
  if (i >= 1 .and. i <= len(text)) ch = text(i:i)
 end function

 logical function space(c)
  character, intent(in) :: c
  space = index(' '//achar(9)//achar(10)//achar(11)//achar(12)//achar(13), c) /= 0
 end function

 subroutine xml_load(path, doc)
  character(len=*), intent(in) :: path
  type(xml_doc), intent(out) :: doc
  integer :: u, ierr, nbytes
  doc%path = path
  doc%text = ''
  doc%tree_error = ''
  allocate(doc%el(16))
  open(newunit=u, file=path, access='stream', form='unformatted', status='old', action='read', iostat=ierr)
  if (ierr /= 0) return
  inquire(unit=u, size=nbytes, iostat=ierr)
  if (ierr == 0) then
   doc%text = repeat(' ', nbytes)
   if (nbytes > 0) read(u, iostat=ierr) doc%text
  end if
  close(u)
  if (ierr /= 0) return
  doc%read_ok = .true.
  doc%tree_ok = .true.
  call read_tree(doc)
 end subroutine

 subroutine tree_error(doc, detail)
  type(xml_doc), intent(inout) :: doc
  character(len=*), intent(in) :: detail
  if (doc%tree_ok) doc%tree_error = detail
  doc%tree_ok = .false.
 end subroutine

 subroutine read_tree(doc)
  type(xml_doc), intent(inout) :: doc
  type(xml_element), allocatable :: grown(:)
  type(xml_attribute), allocatable :: more(:)
  type(xml_attribute) :: attr
  integer :: m, have
  integer :: i, j, first, parent, node, line, k, brackets
  character :: q, c
  character(len=:), allocatable :: name, raw
  logical :: equals, closed
  i = 1
  parent = 0
  do while (i <= len(doc%text))
   if (ch(doc%text,i) /= '<') then
    i = i + 1
    cycle
   end if
   if (doc%text(i:min(i+3,len(doc%text))) == '<!--') then
    ! ezxml takes the first '--' after '<!--' and wants '>' right after it (external/ezxml/ezxml.c:569-572):
    ! a comment holding '--' ends the tree there, and MPAS runs on without the streams after it. lean stops.
    j = index(doc%text(i+4:), '--')
    if (j == 0 .or. ch(doc%text,i+j+5) /= '>') then
     call tree_error(doc, 'unclosed <!--')
     return
    end if
    i = i + j + 6
    cycle
   end if
   if (doc%text(i:min(i+8,len(doc%text))) == '<![CDATA[') then
    j = index(doc%text(i+9:), ']]>')
    if (j == 0) then
     call tree_error(doc, 'unclosed <![CDATA[')
     return
    end if
    i = i + j + 11
    cycle
   end if
   if (ch(doc%text,i+1) == '?') then
    j = index(doc%text(i+2:), '?>')
    if (j == 0) then
     call tree_error(doc, 'unclosed <?')
     return
    end if
    i = i + j + 3
    cycle
   end if
   if (doc%text(i:min(i+8,len(doc%text))) == '<!DOCTYPE') then
    brackets = 0
    do
     i = i + 1
     c = ch(doc%text,i)
     if (i > len(doc%text)) then
      call tree_error(doc, 'unclosed <!DOCTYPE')
      return
     end if
     if (c == '[') brackets = brackets + 1
     if (c == ']') brackets = brackets - 1
     if (c == '>' .and. brackets == 0) exit
    end do
    i = i + 1
    cycle
   end if
   i = i + 1
   if (ch(doc%text,i) == '/') then
    first = i + 1
    i = first
    do while (.not. space(ch(doc%text,i)) .and. ch(doc%text,i) /= '>' .and. i <= len(doc%text))
     i = i + 1
    end do
    name = doc%text(first:i-1)
    if (parent == 0) then
     call tree_error(doc, 'unexpected closing tag '//name)
     return
    end if
    if (name /= doc%el(parent)%name) then
     call tree_error(doc, 'unexpected closing tag '//name)
     return
    end if
    parent = doc%el(parent)%parent
    j = index(doc%text(i:), '>')
    if (j == 0) exit
    i = i + j
    cycle
   end if
   if (parent == 0 .and. doc%n > 0) then
    call tree_error(doc, 'markup outside of root element')
    return
   end if
   c = ch(doc%text,i)
   if (.not. ((c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z') .or. &
              c == '_' .or. c == ':' .or. iachar(c) > 127)) then
    call tree_error(doc, 'unexpected <')
    return
   end if
   first = i
   do while (.not. space(ch(doc%text,i)) .and. index('/>',ch(doc%text,i)) == 0 .and. i <= len(doc%text))
    i = i + 1
   end do
   if (doc%n == size(doc%el)) then
    allocate(grown(2*size(doc%el)))
    grown(1:doc%n) = doc%el(1:doc%n)
    call move_alloc(grown, doc%el)
   end if
   doc%n = doc%n + 1
   node = doc%n
   doc%el(node)%name = doc%text(first:i-1)
   doc%el(node)%parent = parent
   allocate(doc%el(node)%attr(0))
   do
    do while (space(ch(doc%text,i)))
     i = i + 1
    end do
    c = ch(doc%text,i)
    if (index('/>',c) /= 0 .or. i > len(doc%text)) exit
    first = i
    do while (.not. space(ch(doc%text,i)) .and. index('=/>',ch(doc%text,i)) == 0 .and. i <= len(doc%text))
     i = i + 1
    end do
    if (i == first) then
     call tree_error(doc, 'invalid attribute name')
     return
    end if
    attr%name = doc%text(first:i-1)
    attr%value = ''
    equals = .false.
    do while (space(ch(doc%text,i)) .or. ch(doc%text,i) == '=')
     if (ch(doc%text,i) == '=') equals = .true.
     i = i + 1
    end do
    q = ch(doc%text,i)
    if (q == '"' .or. q == "'") then
     i = i + 1
     j = index(doc%text(i:), q)
     if (j == 0) then
      call tree_error(doc, 'missing '//q)
      return
     end if
     raw = doc%text(i:i+j-2)
     attr%value = decode(raw)
     i = i + j
    else if (equals) then
     line = 1
     do k = 1, first-1
      if (ch(doc%text,k) == achar(10)) line = line + 1
     end do
     call tree_error(doc, 'In file '//doc%path//', line '//int_text(line)// &
       ': the value of attribute "'//attr%name//'" is not in quotes.')
    end if
    ! Grown member by member: nvfortran 25.3 loses the deferred lengths in [array, element] of this type.
    have = 0
    if (allocated(doc%el(node)%attr)) have = size(doc%el(node)%attr)
    allocate(more(have+1))
    do m = 1, have
     more(m)%name = doc%el(node)%attr(m)%name
     more(m)%value = doc%el(node)%attr(m)%value
    end do
    more(have+1)%name = attr%name
    more(have+1)%value = attr%value
    call move_alloc(more, doc%el(node)%attr)
   end do
   closed = ch(doc%text,i) == '/'
   if (closed) i = i + 1
   if (ch(doc%text,i) /= '>') then
    call tree_error(doc, 'missing >')
    return
   end if
   if (.not. closed) parent = node
   i = i + 1
  end do
  if (doc%n == 0) then
   call tree_error(doc, 'root tag missing')
  else if (parent /= 0) then
   call tree_error(doc, 'unclosed tag <'//doc%el(parent)%name//'>')
  end if
 end subroutine

 function decode(raw) result(value)
  character(len=*), intent(in) :: raw
  character(len=:), allocatable :: value, token, replacement, normalized, joined
  integer :: i, j, code, ierr
  ! ezxml_decode normalizes CRLF before decoding, and leaves decoded character references alone.
  normalized = ''
  i = 1
  do while (i <= len(raw))
   if (ch(raw,i) == achar(13)) then
    normalized = normalized//achar(10)
    if (ch(raw,i+1) == achar(10)) i = i + 1
   else
    normalized = normalized//ch(raw,i)
   end if
   i = i + 1
  end do
  value = ''
  i = 1
  do while (i <= len(normalized))
   if (ch(normalized,i) == '&') then
    j = index(normalized(i:), ';')
    if (j /= 0) then
     token = normalized(i+1:i+j-2)
     replacement = ''
     select case (token)
     case ('lt'); replacement = '<'
     case ('gt'); replacement = '>'
     case ('amp'); replacement = '&'
     case ('apos'); replacement = "'"
     case ('quot'); replacement = '"'
     end select
     if (ch(token,1) == '#') then
      ierr = 1
      code = 0
      if (ch(token,2) == 'x') then
       read(token(3:), '(z16)', iostat=ierr) code
      else
       read(token(2:), *, iostat=ierr) code
      end if
      if (ierr == 0 .and. code > 0) replacement = utf8(code)
      if (len(replacement) > 0) then
       value = value//replacement
       i = i + j
       cycle
      end if
     else if (len(replacement) > 0) then
      ! Through a second variable: nvfortran 25.3 pads the result when the left side is also on the right.
      joined = normalized(:i-1)//replacement//normalized(i+j:)
      normalized = joined
      cycle
     end if
    end if
   end if
   if (space(ch(normalized,i))) then
    value = value//' '
   else
    value = value//ch(normalized,i)
   end if
   i = i + 1
  end do
 end function

 function utf8(code) result(text)
  integer, intent(in) :: code
  character(len=:), allocatable :: text
  integer :: n, v, j
  if (code < 128) then
   ! Not text = achar(code): nvfortran 25.3 gives that result the length 'code'.
   text = ' '
   text(1:1) = achar(code)
   return
  end if
  n = 2
  v = code / 2048
  do while (v > 0 .and. n < 6)
   n = n + 1
   v = v / 32
  end do
  text = repeat(' ', n)
  v = code
  do j = n, 2, -1
   text(j:j) = achar(128 + iand(v,63))
   v = ishft(v,-6)
  end do
  text(1:1) = achar(256 - 2**(8-n) + v)
 end function

 logical function xml_attr(doc, i, name, value) result(found)
  type(xml_doc), intent(in) :: doc
  integer, intent(in) :: i
  character(len=*), intent(in) :: name
  character(len=:), allocatable, intent(out) :: value
  integer :: j
  found = .false.
  value = ''
  if (i < 1 .or. i > doc%n) return
  do j = 1, size(doc%el(i)%attr)
   if (doc%el(i)%attr(j)%name /= name) cycle
   value = doc%el(i)%attr(j)%value
   found = .true.
   return
  end do
 end function

 function xml_children(doc, i, name) result(idx)
  type(xml_doc), intent(in) :: doc
  integer, intent(in) :: i
  character(len=*), intent(in) :: name
  integer, allocatable :: idx(:)
  integer :: j
  idx = pack([(j, j=1,doc%n)], &
         [(doc%el(j)%parent == i .and. doc%el(j)%name == name, j=1,doc%n)])
 end function

 logical function xml_syntax_ok(doc, msgs) result(ok)
  type(xml_doc), intent(in) :: doc
  type(message_list), intent(inout) :: msgs
  integer :: i, j, nleft, nright, nleftcom, nrightcom, line, first, top, start_line
  integer, allocatable :: stack_line(:)
  type(xml_attribute), allocatable :: stack(:)
  character(len=:), allocatable :: tag, name
  character :: c
  ok = .false.
  if (.not. doc%read_ok) return
  nleft = 0
  nright = 0
  nleftcom = 0
  nrightcom = 0
  line = 1
  if (ch(doc%text,1) == '>') then
   call err("line 1, unexpected starting '>' character. A file cannot start with a  '>' character.")
   return
  end if
  do i = 1, len(doc%text)
   c = ch(doc%text,i)
   if (c == '<') then
    if (ch(doc%text,i+1) == '!') then
     nleftcom = nleftcom + 1
     if (nleftcom - nrightcom > 1) then
      call err('line '//int_text(line)//", unexpected XML comment open. Is the previous XML comment missing a '-->'?")
      return
     else if (nleft /= nright) then
      call err('line '//int_text(line)//", unexpected XML comment open. Is the previous XML tag missing a '>'?"// &
        achar(10)//'   NOTE: Comments are not allowed within an open XML tag.')
      return
     end if
    else
     nleft = nleft + 1
     if (nleft - nright > 1) then
      call err('line '//int_text(line)//", unexpected '<' character. Is the previous XML tag missing a '>'?")
      return
     end if
    end if
   else if (c == '>') then
    if (ch(doc%text,i-1) == '-') then
     nrightcom = nrightcom + 1
     if (nleftcom /= nrightcom) then
      call err('line '//int_text(line)//", unexpected XML comment close. Is the XML comment missing a '<!--'?")
      return
     else if (nleft /= nright) then
      call err('line '//int_text(line)//", unexpected XML comment close. Is the previous XML tag missing a '>'?"// &
        achar(10)//'   NOTE: Comments are not allowed within an open XML tag.')
      return
     end if
    else
     nright = nright + 1
     if (nleft /= nright) then
      call err('line '//int_text(line)//", unexpected '>' character. Is the XML tag missing a '<'?")
      return
     end if
    end if
   else if (c == achar(10)) then
    line = line + 1
   end if
  end do
  if (nleft /= nright) then
   call err("unbalanced angle brackets in XML. Is the file missing a final '>'?")
   return
  end if
  nleft = 0
  line = 1
  do i = 1, len(doc%text)
   c = ch(doc%text,i)
   if (c == '"') nleft = mod(nleft+1,2)
   if (c == '=' .or. c == achar(10)) then
    if (nleft /= 0) then
     call err('line '//int_text(line)//', unterminated string. Is a closing quote not present?')
     return
    end if
    if (c == achar(10)) line = line + 1
   end if
  end do
  if (nleft /= 0) then
   call err('unbalanced quotes in XML.')
   return
  end if
  allocate(stack(len(doc%text)), stack_line(len(doc%text)))
  top = 0
  line = 1
  i = 1
  do while (i <= len(doc%text))
   if (ch(doc%text,i) /= '<') then
    if (ch(doc%text,i) == achar(10)) line = line + 1
    i = i + 1
    cycle
   end if
   start_line = line
   if (doc%text(i:min(i+3,len(doc%text))) == '<!--') then
    j = index(doc%text(i+4:), '-->')
    if (j == 0) exit
    first = i + j + 6
    do while (i < first)
     if (ch(doc%text,i) == achar(10)) line = line + 1
     i = i + 1
    end do
    cycle
   end if
   first = i + 1
   j = index(doc%text(first:), '>')
   if (j <= 1) exit
   i = first + j
   tag = doc%text(first:i-2)
   do j = 1, len(tag)
    if (ch(tag,j) == achar(10)) line = line + 1
   end do
   if (ch(tag,1) == '!' .and. ch(tag,len(tag)) == '-') cycle
   if (ch(tag,len(tag)) == '/') cycle
   name = tag
   if (ch(tag,1) == '/') name = tag(2:)
   j = index(name,' ')
   if (j > 0) name = name(:j-1)
   if (ch(tag,1) /= '/') then
    top = top + 1
    stack(top)%name = name
    stack_line(top) = start_line
   else
    if (top == 0) then
     ! lean stops where MPAS dereferences an empty stack (xml_stream_parser.c:743).
     call err('Found unexpected closing tag "'//name//'" at line '//int_text(start_line)//'.')
     return
    end if
    if (name /= stack(top)%name) then
     call err('Found unexpected closing tag "'//name//'" at line '//int_text(start_line)//'.')
     call err('line '//int_text(stack_line(top))//', unclosed or badly nested XML tag "'//stack(top)%name//'".')
     return
    end if
    top = top - 1
   end if
  end do
  if (top > 0) then
   call err('line '//int_text(stack_line(top))//', unclosed or badly nested XML tag "'//stack(top)%name//'".')
   return
  end if
  ok = .true.
 contains
  subroutine err(text)
   character(len=*), intent(in) :: text
   call add_message(msgs, LOG_ERR, 'In file '//doc%path//', '//text)
  end subroutine
 end function
end module

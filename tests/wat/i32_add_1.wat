(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func))
  (func (;0;) (type 0)
    i32.const 1
    i32.const 4
    i32.add)
  (func (;1;) (type 1)
    call 0
    drop)
  (start 1))

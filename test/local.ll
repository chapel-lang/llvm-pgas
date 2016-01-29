; RUN: opt --load %bindir/lib/llvm-pgas${MOD_EXT} -locality-opt -S < %s | FileCheck %s

target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128-p100:64:64:64"

; for Chapel Array
%atomicflag = type { i8 }
%atomic_int64 = type { i64 }
%chpl_object_object = type { i32, i32 }
%range_int64_t_bounded_F = type { %rangeBase_int64_t_bounded_F, i8 }
%rangeBase_int64_t_bounded_F = type { i64, i64, i64, i64 }
%chpl_DefaultDist_object = type { %chpl_BaseDist_object }
%chpl_BaseDist_object = type { %chpl_object_object, %atomic_int64, %list_BaseDom, %atomicflag }
%list_BaseDom = type { %chpl_listNode_BaseDom_object addrspace(100)*, %chpl_listNode_BaseDom_object addrspace(100)*, i64 }
%chpl_listNode_BaseDom_object = type { %chpl_object_object, %chpl_BaseDom_object addrspace(100)*, %chpl_listNode_BaseDom_object addrspace(100)* }
%chpl_BaseDom_object = type { %chpl_object_object, %atomic_int64, %list_BaseArr, %atomicflag }
%chpl_DefaultRectangularDom_1_int64_t_F_object = type { %chpl_BaseRectangularDom_object, %chpl_DefaultDist_object addrspace(100)*, [1 x %range_int64_t_bounded_F] }
%chpl_BaseRectangularDom_object = type { %chpl_BaseDom_object }
%chpl_BaseArr_object = type { %chpl_object_object, %atomic_int64, %chpl_BaseArr_object addrspace(100)* }
%chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object = type { %chpl_BaseArr_object, %chpl_DefaultRectangularDom_1_int64_t_F_object addrspace(100)*, [1 x i64], [1 x i64], [1 x i64], i64, i64, i64 addrspace(100)*, i64 addrspace(100)*, i8 }
%chpl___RuntimeTypeInfo8 = type { %chpl_DefaultRectangularDom_1_int64_t_F_object addrspace(100)* }
%list_BaseArr = type { %chpl_listNode_BaseArr_object addrspace(100)*, %chpl_listNode_BaseArr_object addrspace(100)*, i64 }
%chpl_listNode_BaseArr_object = type { %chpl_object_object, %chpl_BaseArr_object addrspace(100)*, %chpl_listNode_BaseArr_object addrspace(100)* }

declare void @chpl__buildDomainExpr(%range_int64_t_bounded_F %_e0_ranges.val, %chpl_DefaultRectangularDom_1_int64_t_F_object addrspace(100)** nocapture %_retArg, i64 %_ln, i8* %_fn)
declare void @chpl__ensureDomainExpr3(%range_int64_t_bounded_F* %_e0_x, %chpl_DefaultRectangularDom_1_int64_t_F_object addrspace(100)** %_retArg, i64 %_ln, i8* %_fn)
declare %chpl___RuntimeTypeInfo8 @chpl__buildArrayRuntimeType6(%chpl_DefaultRectangularDom_1_int64_t_F_object addrspace(100)** %dom, i64 %_ln, i8* %_fn)
declare void @chpl__convertRuntimeTypeToValue8(%chpl_DefaultRectangularDom_1_int64_t_F_object addrspace(100)** %dom, %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object addrspace(100)** %_retArg, i64 %_ln, i8* %_fn)
declare void @_build_range(i64 %low, i64 %high2, %range_int64_t_bounded_F* %_retArg, i64 %_ln, i8* %_fn)
declare i64* @.gf.addr.1(i64 addrspace(100)*) readnone
declare %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object* @.gf.addr.2(%chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object addrspace(100)*) readnone

; proc localizeByLocalStmt(ref x) : int {
;    var p: int = 1;
;    local { p = x; }
;    return p + x;
; }

define i64 @localizeByLocalStmt(i64 addrspace(100)* %x) {
; CHECK: @localizeByLocalStmt(
; )
entry:
  %0 = call i64* @.gf.addr.1(i64 addrspace(100)* %x)
  %1 = load i64, i64* %0
; CHECK: call i64* @.gf.addr.
; CHECK; load i64, i64*
; CHECK: add i64
; CHECK: ret i64
  %2 = load i64, i64 addrspace(100)* %x
  %3 = add i64 %2, %1
  ret i64 %3
}

; proc localizeByArrayDecl () {
;  var A: [1..10] int;
;  return A(5);
; }

define internal i64 @localizeByArrayDecl() {
; CHECK: @localizeByArrayDecl(
; )
entry:
  %type_tmp = alloca %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object addrspace(100)*
  store %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object addrspace(100)* null, %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object addrspace(100)** %type_tmp
  %call_tmp = alloca %range_int64_t_bounded_F
  %call_tmp2 = alloca %chpl_DefaultRectangularDom_1_int64_t_F_object addrspace(100)*
  store %chpl_DefaultRectangularDom_1_int64_t_F_object addrspace(100)* null, %chpl_DefaultRectangularDom_1_int64_t_F_object addrspace(100)** %call_tmp2
  %_runtime_type_tmp_ = alloca %chpl_DefaultRectangularDom_1_int64_t_F_object addrspace(100)*
  %_fn = alloca i8
  store %chpl_DefaultRectangularDom_1_int64_t_F_object addrspace(100)* null, %chpl_DefaultRectangularDom_1_int64_t_F_object addrspace(100)** %_runtime_type_tmp_
  call void @_build_range(i64 1, i64 10, %range_int64_t_bounded_F* %call_tmp, i64 9, i8* %_fn)
  call void @chpl__ensureDomainExpr3(%range_int64_t_bounded_F* %call_tmp, %chpl_DefaultRectangularDom_1_int64_t_F_object addrspace(100)** %call_tmp2, i64 9, i8* %_fn)
  %0 = call %chpl___RuntimeTypeInfo8 @chpl__buildArrayRuntimeType6(%chpl_DefaultRectangularDom_1_int64_t_F_object addrspace(100)** %call_tmp2, i64 9, i8* %_fn)
  %.fca.0.extract = extractvalue %chpl___RuntimeTypeInfo8 %0, 0
  store %chpl_DefaultRectangularDom_1_int64_t_F_object addrspace(100)* %.fca.0.extract, %chpl_DefaultRectangularDom_1_int64_t_F_object addrspace(100)** %_runtime_type_tmp_
  call void @chpl__convertRuntimeTypeToValue8(%chpl_DefaultRectangularDom_1_int64_t_F_object addrspace(100)** %_runtime_type_tmp_, %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object addrspace(100)** %type_tmp, i64 9, i8* %_fn)
  %1 = load %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object addrspace(100)*, %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object addrspace(100)** %type_tmp
; CHECK: call %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object* @.gf.addr
; CHECK: getelementptr inbounds %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object, %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object*
  %2 = getelementptr inbounds %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object, %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object addrspace(100)* %1, i32 0, i32 3
; CHECK: getelementptr inbounds [1 x i64], [1 x i64]*  
  %3 = getelementptr inbounds [1 x i64], [1 x i64] addrspace(100)* %2, i64 0, i64 0
; CHECK: load i64, i64*
  %4 = load i64, i64 addrspace(100)* %3
  %5 = mul i64 5, %4
; CHECK: getelementptr inbounds %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object, %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object*  
  %6 = getelementptr inbounds %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object,  %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object addrspace(100)* %1, i32 0, i32 8
; CHECK: load i64 addrspace(100)*, i64 addrspace(100)**
  %7 = load i64 addrspace(100)*, i64 addrspace(100)* addrspace(100)* %6
; CHECK: call i64* @.gf.addr.
; CHECK: getelementptr inbounds i64, i64*
  %8 = getelementptr inbounds i64, i64 addrspace(100)* %7, i64 %5
; CHECK: load i64, i64*
  %9 = load i64, i64 addrspace(100)* %8
; CHECK: ret i64  
  ret i64 %9
}

; proc localizeByGVN(A) : int {
;    A(1) = 1; // local
;    local { A(1) = 2; }
;    A(2) = 3; // non-local
; }

define internal fastcc void @localizeByGVN(%chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object addrspace(100)* %A.val) #1 {
; CHECK: @localizeByGVN(
; )
entry:
; CHECK: call %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object* @.gf.addr.
; CHECK: getelementptr inbounds %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object, %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object*
  %0 = getelementptr inbounds %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object, %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object addrspace(100)* %A.val, i64 0, i32 3, i64 0
; CHECK: load i64, i64*  
  %1 = load i64, i64 addrspace(100)* %0
; CHECK: call %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object* @.gf.addr.  
  %2 = getelementptr inbounds %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object, %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object addrspace(100)* %A.val, i64 0, i32 8
; CHECK: load i64 addrspace(100)*, i64 addrspace(100)**
  %3 = load i64 addrspace(100)*, i64 addrspace(100)* addrspace(100)* %2  
  %4 = getelementptr inbounds i64, i64 addrspace(100)* %3, i64 %1
; CHECK: store i64 1, i64*
  store i64 1, i64 addrspace(100)* %4
  %5 = tail call %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object* @.gf.addr.2(%chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object addrspace(100)* %A.val)
  %6 = getelementptr inbounds %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object, %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object* %5, i64 0, i32 3, i64 0
  %7 = load i64, i64* %6
  %8 = getelementptr inbounds %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object, %chpl_DefaultRectangularArr_int64_t_1_int64_t_F_object* %5, i64 0, i32 8
  %9 = load i64 addrspace(100)*, i64 addrspace(100)** %8
  %10 = tail call i64* @.gf.addr.1(i64 addrspace(100)* %9)
  %11 = getelementptr inbounds i64, i64* %10, i64 %7
  store i64 2, i64* %11
  %12 = load i64, i64 addrspace(100)* %0
  %13 = shl i64 %12, 1
  %14 = getelementptr inbounds i64, i64 addrspace(100)* %3, i64 %13
  store i64 3, i64 addrspace(100)* %14
  ret void
}

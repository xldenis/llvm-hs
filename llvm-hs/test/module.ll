declare !foo !0 !bar !{!"baz"} !qux !{!2} void @f1()

define void @f2() !foo !0 !bar !{!"baz"} !qux !{!2} {
  ret void
}

; --- [ Metadata definitions ] -------------------------------------------------

; Empty named metadata definition.
!foo = !{}

; Plain named metadata definition.
!bar = !{!0}

; Multiple metadata IDs.
!baz = !{!0, !1, !15}

; !vars = !{!9, !10, !11}
; Empty metadata definition.
!0 = !{}

; Plain metadata definition.
!1 = !{!0}

; Multiple metadata IDs.
!2 = !{!0, !1}

; Distinct.
!3 = distinct !{!2}

; Nested metadata.
!4 = !{!{!{!0}}}

; Metadata string.
!5 = !{!"foo"}

; Metadata constant.
!6 = !{i32 42}

; Metadata constant.
!7 = !{!{!"bar"}}

!15 = !{ !9, !10, !8, !11}

!8 = distinct !DISubprogram(name: "foo", scope: !9)
!9 = !DIFile(filename: "path/to/file", directory: "/path/to/dir")

; CHECK: !3 = !DILexicalBlock(scope: !1, file: !2, line: 7, column: 35)
!10 = !DILexicalBlock(scope: !8, file: !9, line: 7, column: 35)

!11 = !DINamespace(name: "Namespace", scope: !10)
; !9 = !DILocalVariable(name: "this", arg: 1, scope: !3, file: !2, line: 7,
;                       type: !3, flags: DIFlagArtificial)
; !10 = !DILocalVariable(name: "x", arg: 2, scope: !4, file: !2, line: 7,
;                       type: !3)
; !11 = !DILocalVariable(name: "y", scope: !5, file: !2, line: 7, type: !3)


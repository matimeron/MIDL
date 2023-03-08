pro HAK, mesg

; NAME:
;       HAK
; PURPOSE:
;       HAK is a procedure that performs "Hit any key to continue".  It waits
;       for keyboard input, clears the type-ahead buffer and allows the
;       application to continue.

;
; Check for the message keyword.
;
if n_elements(mesg) eq 0 then begin
   print, 'Hit any key to continue...'
endif else begin                  ; Print user-defined string
   print, mesg
endelse
;
; Wait for keyboard input before continuing (returning).
;
dumb = get_kbrd (1)
empty
repeat dumb = get_kbrd (0) until dumb eq ''
empty
;
return
end

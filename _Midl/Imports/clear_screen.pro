pro CLEAR_SCREEN

; NAME:
;      CLEAR_SCREEN
; PURPOSE:
;      If the device is a SUN, then clear the alphanumeric window.
;      If the device is a TEK, then clear the alphanumeric plane.
;      If the device is an any other machine (assume VT-compatible), then clear 
;	the alphanumeric window.

; string(27B) is ESC (escape).
; String(12B) is a Form feed(FF).
;
; If the device is a SUN terminal then clear using a FF.
;
if !d.name eq 'SUN' then begin 
  print, string(12B)
endif else begin
;
; Terminal is a TEK or a VT compatible, so use defined clear screen code.
;
  print, string(27B) + '[2J'
endelse
;
return
end

Pro ABC_check, abc

;+
; NAME:
;		ABC_CHECK
; VERSION:
;		8.714
; PURPOSE:
;		Checks for integration consistency in an {ABC} structure.
; CATEGORY:
;		Optics ABC-formalism calculations.
; CALLING SEQUENCE:
;		ABC_CHECK, ABC
; INPUTS:
;	ABC
;		An {ABC} type structure.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 		None.
; OUTPUTS:
;		None other than screen output.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward integration of the data in ABC.  The routine performs
;		integration three different ways, i.e.
;			1) Full integration
;			2) Integrate over space, then angles.
;			3) Integrate over angles, then space.
;		The three results should be the same.
;		Calls ABC_INT (multiple times).
; MODIFICATION HISTORY:
;		Created 15-JUNE-2017 by Mati Meron.
;		Documented 5-JUL-2019 by Mati Meron.
;-

	on_error, 1

	print
	print, ABC_int(abc)
	print, ABC_int(ABC_int(abc,/rad))
	print, ABC_int(ABC_int(abc,/ang))
	print

	return
end
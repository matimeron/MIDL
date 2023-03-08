Pro Complot, x, z, _extra = _e

;+
; NAME:
;		COMPLOT
; VERSION:
;		8.32
; PURPOSE:
;		Plotting complex functions
; CATEGORY:
;		Plotting.
; CALLING SEQUENCE:
;		COMPLOT, [X,] Y [, optional keywords]
; INPUTS:
;	Y
;		Complex vector.  If real vector is given it is taken as the real part of
;		a complex vector, with the imaginary part being identically 0.
; OPTIONAL INPUT PARAMETERS:
;	X
;		A numeric (real) vector containing the X coordinates of the data.  If 
;		absent, it is replaced by the vector [0, 1, 2 ...].
; KEYWORD PARAMETERS:
;	_EXTRA
;		A formal keyword used to pass all plotting keywords.  Not to be used
;		directly.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward. Uses calls to IMAGINARY_MM, PLOTOT and REAL_MM, from
;		MIDL.
; MODIFICATION HISTORY:
;		Created 20-SEP-2014 by Mati Meron.
;-

	on_error, 1
	cols = [!pcol.green,!pcol.blue]

	wx = reform(x)
	case n_params(0) of
		0	:	message, 'Input?'
		1	:	begin
					wx = findgen(n_elements(x))
					wz = reform(x)
				end
		2	:	begin
					wx = reform(x)
					wz = reform(z)
				end
		else:	message, 'Eh?'
	endcase
	Plotot, wx, [[Real_mm(wz)],[Imaginary_mm(wz)]], col = cols, _extra = _e

	return
end
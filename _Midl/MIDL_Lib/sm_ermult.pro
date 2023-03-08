Function Sm_ermult, ker

	;+
	; NAME:
	;		SM_ERMULT
	; VERSION:
	;		8.33
	; PURPOSE:
	;		Calculates the multiplicative factors for smoothed data errors.
	; CATEGORY:
	;		Data Analysis.
	; CALLING SEQUENCE:
	;		Result = SM_ERMULT( KER)
	; INPUTS:
	;	KER
	;		Numerical array, 1 or 2D (usually smoothing kernel).
	; OPTIONAL INPUT PARAMETERS:
	;		None.
	; KEYWORD PARAMETERS:
	;		None.
	; OUTPUTS:
	;		Returns a two-element vector containing the factors for horizontal
	;		(vertically projected) and vertical (horizontally projected) data
	;		errors.
	; OPTIONAL OUTPUT PARAMETERS:
	;		None.
	; COMMON BLOCKS:
	;		None.
	; SIDE EFFECTS:
	;		None.
	; RESTRICTIONS:
	;		None.
	; PROCEDURE:
	;		Following statistical analysis results.  Calls CALCTYPE, CAST and
	;		ISNUM, from MIDL.
	; MODIFICATION HISTORY:
	;		Created 10-OCT-2014 by Mati Meron.
	;-

	on_error, 1

	if Isnum(ker) then begin
		typ = Calctype(ker) > 4
		res = Cast([1.,1.],typ)
		siz = size(ker)
		case siz[0] of
			0	:	
			1	:	res[1] = 1./total(ker^2)
			2	:	begin
						res[0] = 1./total(total(ker,1)^2)
						res[1] = 1./total(total(ker,2)^2)
					end
			else:	message, 'Invalid input!'
		endcase
	endif else message, 'Missing or invalid input!'

	return, sqrt(res)
end
Pro Check_fun, x, y, z, fun_name = fun, epsilon = eps, double = dob, $
    vary = var, detail=det

;+
; NAME:
;	CHECK_FUN
; PURPOSE:
;	Bug swatting.
; CATEGORY:
;
; CALLING SEQUENCE:
;	CHECK_FUN, X [, Y, Z] ,FUN_NAME = FUN, EPSILON = EXP [, optional
;	keywords]
; INPUTS:
;    X, Y, Z
;	Scalar variables.  X is always needed, beyond this it depends on the 
;	function
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    FUN_NAME
;	Mandatory.  The name of the function to be evaluated.
;    EPSILON
;	Increment size
;    /DOUBLE
;	Switch, for those functions which accept a /DOUBLE keyword.
;    VARY
;	Specifies which of the variables is to be varied (thorough an addition 
;	of EPSILON.  By default the first one, X, is being varied.
;    DETAIL
;	Specifies detailed printing.
; OUTPUTS:
;	None other then screen output.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Calculates (FUN(X + EPSILON) - F(X))/EPSILON for few neighboring values 
;	of EPSILON.  The result is a series of numbers which should be nearly
;	the same, other than roundoff errors.  When used with single precision
;	X and with epsilon smaller than minimal FLOAT increment (machine 
;	dependent but EPSILON = 1e-10 is reasonable), the result comes all 
;	zeroes or random.  Changing X to double should fix it.  If it doesn't 
;	it means that the function doesn't handle DOUBLE properly.
; MODIFICATION HISTORY:
;	Created SEP-09-1999 by Mati Meron.
;-

    on_error, 1
    nvar = n_params()
    if n_elements(var) eq 0 then var = 1
    var = 1 > var < nvar


    teps = eps*(1 + findgen(5))
    if keyword_set(dob) then begin
	case nvar of
	    0	:    message, 'Some input needed!'
	    1	:    begin
			fir = call_function(fun,x,/double)
			sec = call_function(fun,x+teps,/double)
		     end
	    2	:    begin
			fir = call_function(fun,x,y,/double)
			if var eq 1 then $
			    sec = call_function(fun,x+teps,y,/double) else $
			    sec = call_function(fun,x,y+teps,/double)
		     end
	    3	:    begin
			fir = call_function(fun,x,y,z,/double)
			if var eq 1 then $
			    sec = call_function(fun,x+teps,y,z,/double) else $
			if var eq 2 then $
			    sec = call_function(fun,x,y+teps,z,/double) else $
			    sec = call_function(fun,x,y,z+teps,/double)
		     end
	    else:    message, 'too many #@%$@&^ variables!'
	endcase
    endif else begin
	case nvar of
	    0	:    message, 'Some input needed!'
	    1	:    begin
			fir = call_function(fun,x)
			sec = call_function(fun,x+teps)
		     end
	    2	:    begin
			fir = call_function(fun,x,y)
			if var eq 1 then $
			    sec = call_function(fun,x+teps,y) else $
			    sec = call_function(fun,x,y+teps)
		     end
	    3	:    begin
			fir = call_function(fun,x,y,z)
			if var eq 1 then $
			    sec = call_function(fun,x+teps,y,z) else $
			if var eq 2 then $
			    sec = call_function(fun,x,y+teps,z) else $
			    sec = call_function(fun,x,y,z+teps)
		     end
	    else:    message, 'too many #@%$@&^ variables!'
	endcase
    endelse

    if keyword_set(det) then begin
	print
	print, 'x = ', x
	if nvar eq 2 then begin
	    print
	    print, 'y = ', y
	endif
	if nvar eq 3 then begin
	    print
	    print, 'z = ', z
	endif
	print
	print, 'epsilon = ', eps
    endif

    if size(x,/type) gt 4 or keyword_set(dob) then vtyp = ', double ' $
	else vtyp = ', single '
    print
    print, 'Results for ' + strupcase(fun) + vtyp + 'precision'
    print, (sec - fir)/teps, form = '(5g15.7)'
    print
    print, string(replicate(45b,80))
    print
    return
end

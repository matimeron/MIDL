Function Wherinarr, arr, subarr, complement = cmp

;+
; NAME:
;		WHERINARR
; VERSION:
;		8.476
; PURPOSE:
;		Locating subarray elements within an array.
; CATEGORY:
;		Mathematical.
; CALLING SEQUENCE:
;		Result = WHERINARR( ARR, SUBARR)
; INPUTS:
;	ARR
;		Numerical array.
;	SUBARR
;		Numerical array, must be a subarray of ARR, i.e. all its elements must
;		be elements of ARR as well.  SUBARR must not have repeated elements.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	COMPLEMENT
;		Optional output, see below.
; OUTPUTS:
;		If SUBARR is a proper subarray of ARR, returns the indices of elements
;		in ARR which are equal to some element of SUBARR. Else, the output is -1
; OPTIONAL OUTPUT PARAMETERS:
;	COMPLEMENT
;		Returns the complement of the result, i.e. the indices of
;		elements in ARR which are not equal to any element of SUBARR.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward evaluation using the system routine VALUE_LOCATE.  
;		Calls INTERSECT_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 20-JAN-2016 by Mati Meron.
;		Modified 5-OCT-2016 by Mati Meron.  Adeed keyword COMPLEMENT.
;-

	on_error, 1

	check = Intersect_mm(arr,subarr,net=nl)
	if nl eq n_elements(subarr) then begin
		s = sort(arr)
		dum = lonarr(n_elements(arr))
		loc = value_locate(arr[s],subarr)
		dum[loc] = 1
		res = where(dum[sort(s)])
		if arg_present(cmp) then cmp = where(dum[sort(s)] eq 0)
	endif else begin
		res = -1l
		if arg_present(cmp) then cmp = -1l
	endelse

	return, res
end
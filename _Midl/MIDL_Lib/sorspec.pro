Function Sorspec, arr, no_sort = nos, complement = cmp, netlen = nl

;+
; NAME:
;		SORSPEC
; VERSION:
;		8.476
; PURPOSE:
;		Similar to the SORT function, but ignores all repeated elements (or
;		alternatively, all non-repeated elements).
; CATEGORY:
;		Array Manipulation.
; CALLING SEQUENCE:
;		Result = SORSPEC ( ARR [, keywords])
; INPUTS:
;	ARR
;		The array to be sorted (scalar is also accepted).
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/NO_SORT
;		Switch.  If set, prevents presorting.  For internal use, on already
;		sorted arrays.
;	/COMPLEMENT
;		Switch.  Reverses the function operation, see outputs below.
;	NETLEN
;		Optional output, see below.
; OUTPUTS:
;		Returns a vector of indices that allow access to all the elements
;		of ARR appearing singly, in ascending order.  Alternatively, if
;		COMPLEMENT is set, returns a vector of indices for all the elements
;		of ARR appearing more than once (but only one index per value).
; OPTIONAL OUTPUT PARAMETERS:
;	NETLEN
;		The name of a variable to receive the number of indices returned.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Uses SORT to order the array, then searches for all the single (or
;		multiple) elements.  Calls SORPURGE and TYPE from MIDL.
; MODIFICATION HISTORY:
;		Created 15-OCT-2016 by Mati Meron, as variation on SORPURGE.
;-

	on_error, 1

	nl = n_elements(arr)
	if nl gt 0 then begin
		if keyword_set(nos) then sor = lindgen(nl) else sor = sort(arr)
		war = arr[sor]
		if Type(war) ne 7 then war = [war[0]-1,war,war[-1]+1] $
		else war = [string(byte(war[0])-1b),war,string(byte(war[-1])+1b)]
		if keyword_set(cmp) then begin
			tor = where(war[1:-2] eq war[0:-3] or war[2:-1] eq war [1:-2], nl)
			if nl gt 0 then tor = tor[Sorpurge(war[tor+1],net=nl)] else tor = []
		endif else begin
			tor = where(war[1:-2] ne war[0:-3] and war[2:-1] ne war [1:-2], nl)
			if nl eq 0 then tor = []
		endelse
	endif else tor = []

	return, sor[tor]
end
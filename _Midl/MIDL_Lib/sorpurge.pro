Function Sorpurge, arr, no_sort = nos, netlen = nl

;+
; NAME:
;		SORPURGE
; VERSION:
;		8.476
; PURPOSE:
;		Similar to the SORT function, but ignores repeated values of elements.
; CATEGORY:
;		Array Manipulation.
; CALLING SEQUENCE:
;		Result = SORPURGE ( ARR [, keywords])
; INPUTS:
;	ARR
;		The array to be sorted (scalar is also accepted).
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	NO_SORT
;		Switch.  If set, prevents presorting.  For internal use, on already
;		sorted arrays.
;	NETLEN
;		Optional output, see below.
; OUTPUTS:
;		Returns a vector of indices that allow access to all DIFFERENT elements
;		of ARR in ascending order.
; OPTIONAL OUTPUT PARAMETERS:
;	NETLEN
;		The name of a variable to receive the number of elements .
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Uses SORT to order the array, then finds all the elements that differ
;		from their predecessors.  Calls TYPE from MIDL.
; MODIFICATION HISTORY:
;		Created 15-AUG-1992 by Mati Meron.
;		Checked for operation under Windows, 30-JAN-2001, by Mati Meron.
;		Modified 20-NOV-2003 by Mati Meron.  Added keyword NO_SORT.
;		Modified 25-SEP-2007 by Mati Meron.  Internal changes.
;		Modified 5-OCT-2012 by Mati Meron.  Internal changes.
;		Modified 15-FEB-2013 by Mati Meron.  Internal changes to accomodate 
;		null input.
;		Modified 15-OCT-2015 by Mati Meron.  Streamlined.
;-

	on_error, 1

	nl = n_elements(arr)
	if nl gt 0 then begin
		if keyword_set(nos) then sorp = lindgen(nl) else sorp = sort(arr)
		if nl gt 1 then begin
			dum = arr[sorp]
			wdum = where((dum[1:nl-1] ne dum[0:nl-2]), ndif)
			if ndif eq 0 then sorp = [0l] else sorp = sorp[[0, 1 + wdum]]
			nl = 1 + ndif
		endif
	endif else sorp = []

	return, sorp
end
Function Hexord, nord, ind = ind, show = sho

;+
; NAME:
;     HEXORD
; VERSION:
;     4.5
; PURPOSE:
;     Calculates indices and squared Q values for diffraction orders of a
;     hexagonal lattice.
; CATEGORY:
;     X-ray calculations.
; CALLING SEQUENCE:
;     Result = HEXORD( NORD [, IND = IND [, /SHOW]]
; INPUTS:
;   NORD
;     The diffraction order(s) for which output is wanted.  Can be given as
;     scalar, or vector with 1, 2 or many elements.  See MAKE_RANGE in MIDL
;     for syntax.
; OPTIONAL INPUT PARAMETERS:
;     None.
; KEYWORD PARAMETERS:
;   IND
;     Optional output, see below.
;   /SHOW
;     Switch.  If set, a table of the orders, their indices and Q^2 values is
;     printed.
; OUTPUTS:
;     Returns the calculated Q^2 values.
; OPTIONAL OUTPUT PARAMETERS:
;   IND
;     Returns the [k,l] indices corresponding to the calculated orders.
; COMMON BLOCKS:
;     None.
; SIDE EFFECTS:
;     None.
; RESTRICTIONS:
;     None.
; PROCEDURE:
;     Evaluates a sufficient number of orders to cover the input list, then
;     prunes the resulting list for output.  Calls DIF, GCD, MAKE_GRID,
;     MAKE_RANGE and TABULATE, from MIDL.
; MODIFICATION HISTORY:
;     Created 25-AUG-2003 by Mati Meron.
;-

    on_error, 1
    lord = max(nord)
    ran = Make_range(nord)
    if lord gt 0 then begin
       llim = floor(sqrt(24/(!pi*sqrt(3))*lord))
       klarr = long(Make_grid([[0,llim],[0,(llim/2)>1]],[1,1],/step))
       k = reform(klarr[0,*,*])
       l = reform(klarr[1,*,*])
       ok = where(l le k/2,nok)
       k = k[ok]
       l = l[ok]
       q = k^2 - k*l + l^2

       s = sort(q)
       k = k[s]
       l = l[s]
       q = q[s]
       s = lindgen(nok)
       dum = where(Dif(q,/forw) eq 0,ndum)
       for i = 0l, ndum-1 do begin
         if k[dum[i]] gt k[dum[i]+1] then begin
         tem = s[dum[i]]
         s[dum[i]] = s[dum[i]+1]
         s[dum[i]+1] = tem
         endif
       endfor
       s = s[ran]
       k = k[s]
       l = l[s]
       q = q[s]
    endif else k = (l = (q = 0l))

    div = GCD(k,l)
    ind = [[k],[l]]
    rind = [[k/(div>1)],[l/(div>1)]]

    if keyword_set(sho) then begin
       sind = strcompress('[' + strjoin(transpose(ind),',') + ']',/rem)
       srind = strcompress(string(div) + $
       '[' + strjoin(transpose(rind),',') + ']',/rem)
       print
       tabulate, ran, sind, srind, q, form = ['i4','i3','a10','i6'], $
       head = ['order #', 'indices', 'red. inds','Q^2'], $
       tit = 'Hexagonal diffraction orders'
       print
    endif

    return, q
end
Function Conlevs, carr, rat, nlev, maxval = cmax, minval = cmin, nozero = noz, $
    ignore_zero = ignoz, show_level = shle, annot = lann, linestyl = lin

    on_error, 1

    rat = Default(rat,2., /dtype) > 2.
    nl = 2 > Default(nlev,8,/dtype) < 24

    if keyword_set(ignoz) then begin
	dum = where(carr gt 0, ndum)
	if ndum eq 0 then begin
	    cmax = Toler(carr)
	    cmin = 0
	endif else cmax = max(carr(dum), min = cmin)
	noz = Default(noz,1,/dtype)
    endif else cmax = max(carr, min = cmin)

    top = floor(alog(cmax)/alog(rat))
    levs = rat^(top + 1 - nl + lindgen(nl))
    lin = lonarr(nl)
    lmax = max(levs)
    ctop = (1. + rat)/2
    if cmax/lmax ge ctop then begin
	lmax = lmax*ctop
	levs = [levs,lmax]
	lin = [lin,0]
	nl = nl + 1
    endif

    if n_elements(shle) ne 0 then begin
	dum = where (shle lt 1 and cmax*shle ge cmin, ndum)
	if ndum gt 0 then begin
	    wshle = shle(dum)
	    levs = [levs,cmax*wshle]
	    lin = [lin,replicate(1l,ndum)]
	    s = sorpurge(levs, netlen = nl)
	    levs = levs(s)
	    lin = lin(s)
	endif
    endif

    mcheck = where(levs gt cmin, mcnum)
    if mcnum ge 3 then begin
	levs = levs(mcheck)
	lin = lin(mcheck)
    endif else begin
	levs = [[0.9,0.5,0.1],[0.1,0.5,0.9]]#[cmin,cmax]
	lin = [0l,0l,0l]
    endelse

    if not keyword_set(noz) and (where(levs eq 0))(0) eq -1 then begin
	levs = [0,levs]
	lin = [0,lin]
    endif

    lann = strtrim(string(levs,format='(g9.3)'),2)

    return, levs
end

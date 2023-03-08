Pro Mpats, npo, ramp, clen, f_order = ford, qrange = qran, qres = qres, $
    cumulative = cum, new_rand = new

    common patstuff, exs, wnpo, wclen, rarr

    ramp = Default(ramp,0.,/dtyp)
    clen = Default(clen,Default(oclen,1l),/dtyp)
    ford = Default(ford,1,/dtyp)
    qran = Default(qran,.1,/dtyp)
    qres = Default(qres,.001,/dtyp)

    if Type(exs) eq 0 or keyword_set(new) then newfl = 1 $
    else if (npo ne wnpo) or (wclen ne clen) then newfl = 1 $
    else newfl = 0

    if newfl then begin
	wnpo = npo
	wclen = clen
	rarr = Randcorr(s,wnpo,wclen)
	exs = 1
    endif

    dr = 1.
    if ramp gt 0 then begin
	if keyword_set(cum) then begin
	    grid = fltarr(npo)
	    for i = 1, npo-1 do grid[i] = grid[i-1] + dr + ramp*rarr[i]
	endif else grid = dr*findgen(npo) + ramp*rarr
	grid = (npo-1)/(max(grid,min=gmin) - gmin)*(grid - gmin)
    endif else grid = dr*findgen(npo) 

    !p.multi = [0,3,2]
    ords = [0l,ford+lindgen(4)]

    plot, grid - findgen(npo)

    for j = 0, 4 do begin
	patt = Diffpat(grid,ords(j)+qran*[-1.,1],qres,qvals=q)
	plot, q/(2*!pi), patt/(1.*npo)^2
    endfor

    return
end    

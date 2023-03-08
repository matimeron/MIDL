Pro Mandscan

    npoints = 101
    zsize = 2.5
    zcent = complex(.75,0)

    tem = zsize/(npoints-1)*findgen(npoints) - zsize/2
    x = float(zcent) + tem
    y = imaginary(zcent) + tem
    z = complexarr(npoints,npoints)
    for i = 0, npoints - 1 do begin
	z(*,i) = complex(x,y(i))
    endfor
    val = lonarr(npoints,npoints)

    for i = 0, npoints - 1 do begin
	print, i
	for j = 0, npoints - 1 do begin
	    val(i,j) = Mandsim(z(i,j),95,64,/nop)
	endfor
    endfor

    openw, unit, 'mand.dat', /get_lun, /block
    dat = assoc(unit,lonarr(npoints,npoints,/nozero))
    dat(0) = val
    free_lun, unit

    return
end

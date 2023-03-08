Pro Mandshow, lev, val

    npoints = 101
    zsize = 2.5
    zcent = complex(.75,0)
    mpoints = 95

    tem = zsize/(npoints-1)*findgen(npoints) - zsize/2
    x = float(zcent) + tem
    y = imaginary(zcent) + tem

    openr, unit, 'mand.dat', /get_lun
    dat = assoc(unit,lonarr(npoints,npoints,/nozero))
    val = float(dat(0))/mpoints^2
    free_lun, unit

    contour, (val*(2*lev - val)/lev) > 0, x, y, yrange = [-1.,1.], $
    /follow, levels = lev*[0.96,0.99,1.00]

    return
end

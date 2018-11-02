pro h5ls, filename
    compile_opt idl2, hidden
    on_error, 2

    savefile = obj_new('h5file', filename, /read)
    savefile->ls
    obj_destroy, savefile
end

;docformat = 'rst'

; h5variable
;
; Based on h5var code by Andrew Collette

;+
; Create a new h5variable object
;
; :Params:
;   group_id : in, required, type=long
;       HDF5 library ID of the group containing the dataset
;   name : in, required, type=string
;       Name of the dataset to open (case-sensitive)
;   data : in, optional, type="{any supported IDL type}"
;       IDL data to put in the dataset, if /CREATE is set
;
; :Keywords:
;   create : in, optional, type=boolean
;       Set this to create a new dataset.  You must specify either "data",
;       or set *both* the keywords "dimensions" and "type"
;   _extra : in, optional, type=keywords
;       Keywords for either h5variable::open or h5variable::create
;-
function h5variable::init, group_id, name, data, create=create,_extra=extra
    compile_opt idl2, hidden
    on_error, 2
    if n_elements(group_id) ne 0 then begin
        if keyword_set(create) then $
            self->create, group_id, name, data, _extra=extra $
        else $
            self->open, group_id, name, _extra=extra
    endif
    ; this should be static
    self.types = [ 'UNDEFINED', 'BYTE', 'INT', 'LONG', 'FLOAT', 'DOUBLE', 'COMPLEX', 'STRING', 'STRUCT', 'DCOMPLEX', 'POINTER', 'OBJREF', 'UINT', 'ULONG', 'LONG64', 'ULONG64' ]
    return, 1
end

;----------------------------------------------------------------------------

;+
; Open a dataset "name" in the group specified by group_id
;
; :Params:
;   group_id : in, required, type=long
;       HDF5 library ID of the group containing the dataset
;   name : in, required, type=string
;       Name of the dataset to open (case-sensitive)
;
; :Keywords:
;   readonly : in, required, type=boolean
;       Set this to open the dataset in read-only mode
;-
pro h5variable::open, group_id, name, readonly=readonly
    compile_opt idl2, hidden
    on_error, 2
    self.group_id = group_id
    self.dataset_name = name
    self.readonly = keyword_set(readonly) 
    self.dataset_id = h5d_open(self.group_id, name)
    self.datatype_id = h5d_get_type(self.dataset_id)
    self.dataspace_id = h5d_get_space(self.dataset_id)
    length = h5s_get_simple_extent_dims(self.dataspace_id)
    self.scalar = array_equal(length, 0)
    if self.scalar then $
        self.memory_space_id = h5s_create_scalar() $
    else $
        self.memory_space_id = h5s_create_simple(length)
end

;----------------------------------------------------------------------------

;+
; Create a new dataset in the HDF5 group corresponding to "group_id", with
; the specified attributes
;
; :Params:
;   group_id : in, required, type=long
;       HDF5 library ID of the group containing the dataset
;   name : in, required, type=string
;       Name of the dataset to open (case-sensitive)
;   data : in, optional, type="{any supported IDL type}"
;       IDL data to put in the dataset.
;
; :Keywords:
;   type : in, optional, type="{any supported IDL type}"
;       Dataset will be created to match the type of this input
;   dimensions : in, optional, type="1D IDL array"
;       Vector containing the dimensions of the new HDF5 dataspace
;   compress : in, optional, type=boolean
;       Compress the data.  If you use this, you must also set
;       chunk_dimensions
;   chunk_dimensions : in, optional, type="1D IDL array"
;       Vector containing chunk specifications.
;   member_names : in, optional, type="unknown"
;       UNDOCUMENTED
;   _extra : in, optional, type=keywords
;       Keywords to pass along to h5variable::write
;-
pro h5variable::create, group_id, name, data, type=type, dimensions=dimensions, compress=compress, chunk_dimensions=chunk_dimensions, member_names=member_names, _extra=write_extra
    compile_opt idl2, hidden
    on_error, 2

    ; try to set a chunk size around 128kb, but no less than 4kb
    chunk_target = 128. * 1024.
    chunk_min = 4. * 1024.

    self.group_id = group_id
    self.dataset_name = name
    self.readonly = 0 

    if (n_elements(data) ne 0) && ((n_elements(type) ne 0) || (n_elements(dimensions) ne 0)) then begin
        message, 'You may only specify TYPE and DIMENSIONS, or DATA, but not both.'
    endif
    
    if (n_elements(data) eq 0) && ((n_elements(type) eq 0) || (n_elements(dimensions) eq 0)) then begin
        message, 'You must specify *either* DATA or both TYPE and DIMENSIONS.'
    endif

    if n_elements(type) ne 0 then begin
        self.datatype_id = H5T_IDL_CREATE(type, member_names=member_names)
    endif 

    if n_elements(data) ne 0 then begin
        self.datatype_id = H5T_IDL_CREATE(data, member_names=member_names)
        dimensions=size(data, /dimensions)
    endif

    if dimensions[0] eq 0 then begin 
        self.dataspace_id = h5s_create_scalar() 
        self.memory_space_id = h5s_create_scalar()
        self.scalar = 1
        self.dataset_id = H5D_CREATE(self.group_id, name, self.datatype_id, self.dataspace_id)
    endif else begin
        self.dataspace_id = h5s_create_simple(dimensions)
        self.memory_space_id = h5s_create_simple(dimensions)
        self.scalar = 0
        ; try to set a squarish chunk size around 128kb, but no less than 4kb
        if n_elements(chunk_dimensions) eq 0 then begin
            chunk_dimensions = dimensions  
            type_size = float(h5t_get_size(self.datatype_id))  
            ; keep dividing the chunk dimensions by 2 until the chunk size is
            ; less than the target, but stop if it will be less than the min.        
            while product(chunk_dimensions)*type_size gt chunk_target do begin
                new_chunk_dimensions = (chunk_dimensions / 2) > 1
                if product(new_chunk_dimensions)*type_size lt chunk_min then $
                    break $
                else $
                    chunk_dimensions = new_chunk_dimensions
            endwhile 
        endif
        if keyword_set(compress) then begin
            gzip = 4
        endif
        self.dataset_id = H5D_CREATE(self.group_id, name, self.datatype_id, self.dataspace_id, CHUNK_DIMENSIONS=chunk_dimensions, GZIP=gzip, shuffle=shuffle)
    endelse

    if n_elements(data) ne 0 then begin
        self->write, data, _extra=write_extra
    endif
end

;----------------------------------------------------------------------------

;+
; Cleanup routine
; :Hidden:
;-
pro h5variable::cleanup
    compile_opt idl2, hidden
    on_error, 2
    self->close
end

;----------------------------------------------------------------------------

;+
; Close the current dataset (so another may be opened)
; Called automatically when destroying the object.
;-
pro h5variable::close
    compile_opt idl2, hidden
    on_error, 2
    h5t_close, self.datatype_id
    h5s_close, self.memory_space_id
    h5s_close, self.dataspace_id
    h5d_close, self.dataset_id
end

;----------------------------------------------------------------------------

;+
; Returns an IDL array with the dataspace dimensions
;-
function h5variable::dims
    compile_opt idl2, hidden
    on_error, 2
    h5s_select_all, self.dataspace_id
    return, h5s_get_simple_extent_dims(self.dataspace_id)
end

;----------------------------------------------------------------------------

;+
; Prints out information about the dataset
;-
pro h5variable::ls
    compile_opt idl2, hidden
    on_error, 2
    
    print, self.dataset_name + ': ' + self.types[h5t_idltype(self.datatype_id)] + ' [' + strjoin(string(self->dims(), format='(i0)'), ', ') + ']'
end

;----------------------------------------------------------------------------

;+
; Returns the size of a dataset in bytes.
;
;   allocated: If set, then the actual number of bytes allocated in the hdf5 file for the dataset is returned.  In some cases this can take a few seconds to calculate.  If not set, then the nominal size of the dataset is returned.  This can be slow if the dataset is highly chunked..
;
;-
function h5variable::size, allocated=allocated
    compile_opt idl2, hidden
    on_error, 2

    if keyword_set(allocated) then $
        return, h5d_get_storage_size(self.dataset_id) $
    else $
        return, product(self->dims())*float(h5t_get_size(self.datatype_id))
end

;----------------------------------------------------------------------------


; Write data to the file
;
; Data:     IDL array to write ( e.g. findgen(100,100,15) )
; Start:    IDL array containing the start position for the write in the file (e.g. [0,0,10,0])
; Length:   IDL array specifying length of write along each dimension of the dataspace (e.g. [100,100,15,1])
;           Cumulative product of this array MUST match the # of elements in "Data".
; Example:
; IDL> print, myhdf->get_dimensions()
;   100 100 5
; IDL> a = findgen(100,100)
; IDL> myhdf->write, a, [0,0,0], [100,100,1]
;
pro h5variable::w, data, start, length, stride, _extra=extra
    compile_opt idl2, hidden
    on_error, 2
    self->write, data, start, length, stride, _extra=extra
end

pro h5variable::write, data, start, length, stride, stop=stop, flush=flush,         select=select
    compile_opt idl2, hidden
    on_error, 2

    if self.readonly then begin
        message, 'File is read-only.'
    endif

    if ~keyword_set(select) then $
        self->select, start, length, stride, stop=stop
    h5d_write, self.dataset_id, data, file_space_id=self.dataspace_id, memory_space_id=self.memory_space_id

    if keyword_set(flush) then begin
        self->close
        self->open, self.group_id, self.dataset_name, readonly=self.readonly
    endif
end

;----------------------------------------------------------------------------
; Read data from the file
; 
; Start:    IDL array containing start positions in HDF5 dataspace for read
; Length:   IDL array containing length of read for each dimension
; r:        Set this keyword to remove all dimensions of length 1 from the returned array.
;
; Example:
; IDL> print, myhdf->get_dimensions()
;   100 100 5
; IDL> out = myhdf->read([0,0,0],[100,100,1],/r)
; IDL> help, out
; OUT             FLOAT     = Array[100, 100]
;
function h5variable::r, start, length, stride, _extra=extra
    compile_opt idl2, hidden
    on_error, 2
    return, self->read(start, length, stride, _extra=extra)
end

function h5variable::read, start, length, stride, stop=stop, reform=reform, select=select, replace=replace, max_size=max_size
    compile_opt idl2, hidden
    on_error, 2

    ; by default, max out at 50% of rocks' 8GB
    if ~ keyword_set(max_size) then max_size = 4.0e9

    if ~keyword_set(select) then $
        self->select, start, length, stride, stop=stop

    data_size = float(h5t_get_size(self.datatype_id)) * h5s_get_select_npoints(self.dataspace_id)
    if (data_size gt max_size) then begin
        ; stop with an error
        message, "The requested slice will require approximately " + string(data_size/1e9, format='(f0.2)') + " gigabytes of memory, which is larger than MAX_SIZE. Reduce the dataset dimensions, or, at your own peril, set the MAX_SIZE keyword to a larger value in bytes."
    endif

    out = h5d_read(self.dataset_id, file_space_id=self.dataspace_id, memory_space_id=self.memory_space_id)

    if keyword_set(reform) then out=reform(temporary(out))

    if keyword_set(replace) then obj_destroy, self

    return, out

end


;----------------------------------------------------------------------------

;+
; Select part of the dataset.
;
; :Params:
;   start : in, optional, type="1D IDL array"
;       Vector containing HDF5 start offset
;   length : in, optional, type="1D IDL array"
;       Vector containing HDF5 length
;   stride : in, optional, type="1D IDL array"
;       Vector containing HDF5 strides for each dimension
;
; :Keywords:
;   reform : in, optional, type=boolean
;       If set, remove all singlet dimensions before returning
;   stop : in, optional, type="unknown"
;       UNDOCUMENTED
;   all : in, optional, type="unknown"
;       UNDOCUMENTED
;-
pro h5variable::select, start, length, stride, stop=stop, all=all
    compile_opt idl2, hidden
    on_error, 2

    if ~self.scalar then begin
        dims = h5s_get_simple_extent_dims(self.dataspace_id)
        ndims = n_elements(dims)
        
        selection = { start: lonarr(ndims), length: lonarr(ndims) + 1, stride: lonarr(ndims) + 1}
    
        if n_elements(start) eq 0 then begin
            selection.length = dims
        endif else begin 
            if n_elements(start) ne ndims then $
                message, 'START must have the length equal to the number of dimensions in the dataset.' $
            else $
                selection.start = long(start)
        endelse
    
        if (n_elements(length) ne 0) then begin 
            if (n_elements(length) ne ndims) then $
                message, 'LENGTH (or STOP) must have the length equal to the number of dimensions in the dataset.' $
            else $
                selection.length = long(length)
        endif
    
        if (n_elements(stride) ne 0) then begin 
            if (n_elements(stride) ne ndims) then $
                message, 'STRIDE must have the length equal to the number of dimensions in the dataset.' $
            else $
                selection.stride = long(stride)
        endif    

        for j = 0, ndims - 1 do begin
            if selection.start[j] eq -1 then begin
                selection.start[j] = 0
                selection.length[j] = -1
            endif
            if selection.length[j] eq -1 then begin
                selection.length[j] = ceil(float(dims[j] - selection.start[j]) / selection.stride[j])
            endif else begin
                if keyword_set(stop) then begin
                    selection.length[j] = ceil(float(selection.length[j] + 1 - selection.start[j]) / selection.stride[j])
                endif else begin
                    selection.length[j] = ceil(float(selection.length[j]) / selection.stride[j])
                endelse
            endelse
        endfor

        h5s_close, self.memory_space_id
        if keyword_set(all) then begin
            h5s_select_all, self.dataspace_id
            self.memory_space_id = h5s_create_simple(h5s_get_simple_extent_dims(self.dataspace_id))
        endif else begin
            h5s_select_hyperslab, self.dataspace_id, selection.start, selection.length, stride=selection.stride, /reset
            self.memory_space_id = h5s_create_simple(selection.length)
        endelse
    endif
end

;----------------------------------------------------------------------------

;+
; Write to a named attribute
;
; :Params:
;   attr_name : in, required, type=string
;       Name of the attribute to write (case-sensitive)
;   value : in, required, type="{any supported IDL type}"
;       Value to write
;
; :Keywords:
;   member_names : in, optional, type="unknown"
;       UNDOCUMENTED
;-
pro h5variable::write_attribute, attr_name, value, member_names=member_names
    compile_opt idl2, hidden
    on_error, 2
    if self.readonly then begin
        message, 'File is read-only.'
    endif
    attr_type = h5t_idl_create(value, member_names=member_names)
    if size(value, /dimensions) ne 0 then $
        attr_space = h5s_create_simple(size(value, /dimensions)) $
    else $
        attr_space = h5s_create_simple([1])

    na = h5a_get_num_attrs(self.dataset_id)
    for j=0, na-1 do begin
        attr_id = h5a_open_idx(self.dataset_id, j)
        if strcmp(attr_name, h5a_get_name(attr_id)) then begin
            h5a_close, attr_id
            h5a_delete, self.dataset_id, attr_name
            break
        endif
        h5a_close, attr_id
    endfor

    attr_id = h5a_create(self.dataset_id, attr_name, attr_type, attr_space)

    h5a_write, attr_id, value
    h5a_close, attr_id
    h5s_close, attr_space
    h5t_close, attr_type
end

;----------------------------------------------------------------------------

;+
; Read a named attribute
; Generates an HDF5 error if the attribute doesn't exist.
;
; :Params:
;   attr_name : in, required, type=string
;       Name of attribute to retrive (case-sensitive)
;-
function h5variable::read_attribute, attr_name, group=group
    compile_opt idl2, hidden
    on_error, 2

    if keyword_set(group) then $
        id = self.group_id $
    else $
        id = self.dataset_id

    attr_id = h5a_open_name(id, attr_name)
    value = h5a_read(attr_id)
    h5a_close, attr_id

    return, value
end

;----------------------------------------------------------------------------

;+
; Retrieve an array of attribute names
;
; :Returns: strarr, or -1 if none are present
;
;-
function h5variable::get_attribute_names, group=group
    compile_opt idl2, hidden
    on_error, 2

    if keyword_set(group) then $
        id = self.group_id $
    else $
        id = self.dataset_id

    na = h5a_get_num_attrs(id)

    if( na eq 0 ) then return, -1   ; bail, as IDL can't do zero-length arrays

    names = strarr(na)

    for attr_idx=0, na-1 do begin

        attr_id = h5a_open_idx(id, attr_idx)
        names[attr_idx] = h5a_get_name(attr_id)
        h5a_close, attr_id

    endfor
    
    return, names

end

;----------------------------------------------------------------------------

;+
; List the attributes attached to a dataset.
;-
pro h5variable::ls_attributes
    compile_opt idl2, hidden
    on_error, 2


    for i = 0, 1 do begin
        if i eq 0 then begin
            id = self.dataset_id
            print, 'Attributes in dataset "' + string(self.dataset_name) + '"'
            print, '-----------------------' + strjoin(replicate('-', strlen(self.dataset_name) + 1))
        endif else begin
            id = self.group_id
            print, 'Attributes in root group'
            print, '------------------------'
        endelse
    
        na = h5a_get_num_attrs(id)
        if na eq 0 then begin
            print, '<none>'
        endif
        for j=0, na-1 do begin
            strvalue = ''
            attr_id = h5a_open_idx(id,j)
            type_id = h5a_get_type(attr_id)
            space_id = h5a_get_space(attr_id)
            typename = self.types[h5t_idltype(type_id)]
            if( h5t_idltype(type_id) eq 7 ) then strvalue = '"'+h5a_read(attr_id)+'"'
            h5s_select_all, self.dataspace_id
            dims = strjoin(string(h5s_get_simple_extent_dims(space_id), format='(i0)'), ', ')
            print, h5a_get_name(attr_id) + ': ' + typename + ' ' + '[' + dims +']' + ' ' + strvalue
            h5t_close, type_id
            h5s_close, space_id
            h5a_close, attr_id
        endfor
        print
    endfor
end

;----------------------------------------------------------------------------

;+
; Class definition
; :Hidden:
;-
pro h5variable__define
    compile_opt idl2, hidden
    on_error, 2

    struct = { h5variable, $
           group_id: long(0), $
           dataset_name: '', $
           datatype_id: 0L, $
           dataspace_id: 0L, $
           scalar: 0L, $
           memory_space_id: 0L, $
           dataset_id: 0L, $
           readonly: fix(0), $
           ; this should be static
           types: strarr(16) $
         }
end

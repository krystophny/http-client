program simple_test
    ! Minimal test program to isolate the segfault
    use iso_c_binding, only: c_ptr, c_associated
    use curl, only: curl_easy_init, curl_easy_cleanup, curl_easy_setopt, CURLOPT_URL
    implicit none
    
    type(c_ptr) :: curl_ptr
    integer :: rc
    
    print *, 'DEBUG: Starting simple curl test'
    
    ! Initialize curl
    curl_ptr = curl_easy_init()
    if (.not. c_associated(curl_ptr)) then
        print *, 'DEBUG: curl_easy_init failed'
        stop 1
    end if
    print *, 'DEBUG: curl_easy_init successful'
    
    ! Set URL
    rc = curl_easy_setopt(curl_ptr, CURLOPT_URL, 'https://httpbin.org/get')
    print *, 'DEBUG: URL set, rc=', rc
    
    ! Cleanup
    print *, 'DEBUG: Cleaning up'
    call curl_easy_cleanup(curl_ptr)
    print *, 'DEBUG: Cleanup done'
    
    print *, 'DEBUG: Program ending normally'
    
end program simple_test
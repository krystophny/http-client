program get_request
    ! This program demonstrates sending a simple GET request and printing the
    ! status, length of the body, method, and the body of the response.
    use http, only: response_type, request
    implicit none
    type(response_type) :: response

    print *, 'DEBUG: Starting get_request program'
    print *, 'DEBUG: About to call request()'
    response = request(url='https://httpbin.org/get')
    print *, 'DEBUG: request() returned'
    
    if(.not. response%ok) then
        print *,'Error message : ', response%err_msg
    else
        print *, 'Response Code    : ', response%status_code
        print *, 'Response Length  : ', response%content_length
        print *, 'Response Method  : ', response%method
        print *, 'Response Content : ', response%content
    end if
    
    ! Explicit cleanup to prevent segfault
    print *, 'DEBUG: Cleaning up response'
    if (allocated(response%content)) then
        print *, 'DEBUG: Deallocating content'
        deallocate(response%content)
    end if
    if (allocated(response%method)) then
        print *, 'DEBUG: Deallocating method'  
        deallocate(response%method)
    end if
    if (allocated(response%url)) then
        print *, 'DEBUG: Deallocating url'
        deallocate(response%url)
    end if
    if (allocated(response%err_msg)) then
        print *, 'DEBUG: Deallocating err_msg'
        deallocate(response%err_msg)
    end if
    if (allocated(response%header)) then
        print *, 'DEBUG: Deallocating header'
        deallocate(response%header)
    end if
    print *, 'DEBUG: Cleanup completed'
    
    print *, 'DEBUG: Program ending'

end program get_request

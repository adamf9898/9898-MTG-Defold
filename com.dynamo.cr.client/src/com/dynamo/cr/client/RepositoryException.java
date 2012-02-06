package com.dynamo.cr.client;

public class RepositoryException extends Exception {

    private int statusCode = -1;

    public RepositoryException(String message, int statusCode) {
        super(message);
        this.statusCode = statusCode;
    }

    public RepositoryException(String message, Throwable e) {
        super(message, e);
        this.statusCode = 400;
    }

    /**
     *
     */
    private static final long serialVersionUID = -7276666163326277589L;

    public int getStatusCode() {
        return statusCode ;
    }

}

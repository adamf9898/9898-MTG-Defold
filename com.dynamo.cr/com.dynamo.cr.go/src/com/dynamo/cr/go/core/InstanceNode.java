package com.dynamo.cr.go.core;

import javax.vecmath.Matrix3d;
import javax.vecmath.Matrix4d;

import org.eclipse.core.runtime.IStatus;

import com.dynamo.cr.properties.NotEmpty;
import com.dynamo.cr.properties.NotZero;
import com.dynamo.cr.properties.Property;
import com.dynamo.cr.sceneed.core.Node;
import com.dynamo.cr.sceneed.core.validators.Unique;

@SuppressWarnings("serial")
public class InstanceNode extends Node {

    @Property
    @NotZero
    private double scale = 1.0;

    @Property
    @NotEmpty(severity = IStatus.ERROR)
    @Unique(scope = InstanceNode.class, base = CollectionNode.class)
    private String id = "";

    public InstanceNode() {
        super();
        setFlags(Flags.TRANSFORMABLE);
        setFlags(Flags.SUPPORTS_SCALE);
    }

    public String getId() {
        return this.id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public double getScale() {
        return this.scale;
    }

    public void setScale(double scale) {
        this.scale = scale;
        setAABBDirty();
        transformChanged();
    }

    @Override
    public void getLocalTransform(Matrix4d transform) {
        super.getLocalTransform(transform);
        transform.setScale(this.scale);
    }

    @Override
    public void setLocalTransform(Matrix4d transform) {
        Matrix4d localTransform = new Matrix4d(transform);
        Matrix3d rotScale = new Matrix3d();
        localTransform.getRotationScale(rotScale);
        this.scale = rotScale.getScale();
        rotScale.normalize();
        localTransform.setRotationScale(rotScale);
        super.setLocalTransform(localTransform);
    }
}

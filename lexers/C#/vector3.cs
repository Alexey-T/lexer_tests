v=/*ddd*/
using System;
using System.Drawing;
using System.Linq;
using System.Collections.Generic;
using System.Globalization;
using Microsoft.Xna;
using OpenTK;
using OpenTK.Graphics;
using OpenTK.Platform;
using OpenTK.Graphics.OpenGL;
using BulletSharp;

namespace Orakel
{
    public struct Vector3
    {
        private float _x;
        private float _y;
        private float _z;

        public float X { get { return _x; } }
        public float Y { get { return _y; } }
        public float Z { get { return _z; } }

        public static Vector3 Zero { get { return new Vector3(0f, 0f, 0f); } }

        /// <summary>
        /// Normalized copy of the vector
        /// </summary>
        public Vector3 Unit
        {
            get
            {
                if (this.Magnitude == 0f)
                {
                    return this;
                }
                return new Vector3(this._x / this.Magnitude, this._y / this.Magnitude, this._z / this.Magnitude);
            }
        }

        /// <summary>
        /// Magnitude of the vector
        /// </summary>
        public float Magnitude
        {
            get { return (float)Math.Sqrt(this.MagnitudeSquared); }
        }

        public float MagnitudeSquared
        {
            get { return this._x * this._x + this._y * this._y + this._z * this._z; }
        }

        /// <summary>
        /// Returns the Dot-product of two vectors
        /// </summary>
        /// <param name="other"></param>
        /// <returns></returns>
        public float Dot(Vector3 other)
        {
            return this.Y * other.Y + this.X * other.X + this.Z * other.Z;
        }

        /// <summary>
        /// Returns the Cross-product of two vectors
        /// </summary>
        /// <param name="other"></param>
        /// <returns></returns>
        public Vector3 Cross(Vector3 other)
        {
            float x, y, z;
            x = this.Y * other.Z - other.Y * other.Z;
            y = (this.X * other.Z - other.X * this.Z) * -1;
            z = this.X * other.Y - other.X * this.Y;

            return new Vector3(x, y, z).Unit;
        }

        /// <summary>
        /// Returns the angle between two vectors, in radians
        /// </summary>
        /// <param name="other"></param>
        /// <returns></returns>
        public float AngleBetween(Vector3 other)
        {
            return (float)Math.Acos(this.Unit.Dot(other.Unit));
        }



        public static Vector3 operator +(Vector3 left, Vector3 right)
        {
            return new Vector3(left.X + right.X, left.Y + right.Y, left.Z + right.Z);
        }

        public static Vector3 operator -(Vector3 left, Vector3 right)
        {
            return new Vector3(left.X - right.X, left.Y - right.Y, left.Z - right.Z);
        }

        public static Vector3 operator *(Vector3 left, Vector3 right)
        {
            return new Vector3(left.X * right.X, left.Y * right.Y, left.Z * right.Z);
        }

        public static Vector3 operator /(Vector3 left, Vector3 right)
        {
            return new Vector3(left.X / right.X, left.Y / right.Y, left.Z / right.Z);
        }

        public static Vector3 operator *(Vector3 source, float scalar)
        {
            return new Vector3(source.X * scalar, source.Y * scalar, source.Z * scalar);
        }

        public static Vector3 operator *(float scalar, Vector3 source)
        {
            return source * scalar;
        }

        public static Vector3 operator /(Vector3 source, float scalar)
        {
            //Let's not implement this using operator* in order to avoid rounding errors
            return new Vector3(source.X / scalar, source.Y / scalar, source.Z / scalar);
        }

        public static Vector3 operator -(Vector3 source)
        {
            return new Vector3(-source.X, -source.Y, -source.Z);
        }

        public override bool Equals(object obj)
        {
            float x, y, z;
            if (obj is Vector3)
            {
                x = ((Vector3)obj).X;
                y = ((Vector3)obj).Y;
                z = ((Vector3)obj).Z;
            }
            else if (obj is Microsoft.Xna.Framework.Vector3)
            {
                x = ((Microsoft.Xna.Framework.Vector3)obj).X;
                y = ((Microsoft.Xna.Framework.Vector3)obj).Y;
                z = ((Microsoft.Xna.Framework.Vector3)obj).Z;
            }
            else if (obj is OpenTK.Vector3)
            {
                x = ((OpenTK.Vector3)obj).X;
                y = ((OpenTK.Vector3)obj).Y;
                z = ((OpenTK.Vector3)obj).Z;
            }
            else
                return false;

            return (Math.Abs(this.X - x) < float.Epsilon) && (Math.Abs(this.Y - y) < float.Epsilon) && (Math.Abs(this.Z - z) < float.Epsilon);
        }

        public override int GetHashCode()
        {
            return ObjectHelper.GetHashCode(X.GetHashCode(), Y.GetHashCode(), Z.GetHashCode());
        }

        public override string ToString()
        {
            return this.X + ", " + this.Y + ", " + this.Z;
        }

        public static bool operator ==(Vector3 left, Vector3 right)
        {
            return (Math.Abs(left.X - right.X) < float.Epsilon) && (Math.Abs(left.Y - right.Y) < float.Epsilon) && (Math.Abs(left.Z - right.Z) < float.Epsilon);
        }

        public static bool operator !=(Vector3 left, Vector3 right)
        {
            return !(left == right);
        }

        public static implicit operator OpenTK.Vector3(Vector3 other)
        {
            return new OpenTK.Vector3(other.X, other.Y, other.Z);
        }

        public static implicit operator Microsoft.Xna.Framework.Vector3(Vector3 other)
        {
            return new Microsoft.Xna.Framework.Vector3(other.X, other.Y, other.Z);
        }

        public static implicit operator Vector3(Microsoft.Xna.Framework.Vector3 other)
        {
            return new Vector3(other.X, other.Y, other.Z);
        }

        public static implicit operator Vector3(OpenTK.Vector3 other)
        {
            return new Vector3(other.X, other.Y, other.Z);
        }



        /// <summary>
        /// Creates a new Vector3 from x,y,z components
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <param name="z"></param>
        public Vector3(float x, float y, float z)
        {
            this._x = x;
            this._y = y;
            this._z = z;
        }
    }
}

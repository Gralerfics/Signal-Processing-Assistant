% TS & FS Class
% By Gralerfics
classdef SP < Spectrum
    methods                                                                 % Member methods.
        %%% Construction methods.
            % SP({l, r}, value, [fs], [unit]) - All parameters.
                % value can be a scalar.
                % [fs = 1], [unit = 1].
                % dw = unit / fs (= w0, when fs = 1).
            % SP({l}, value, [fs], [unit])    - Automatically generate r.
            % SP(value, [fs], [unit])         - Default l = 0.
            % SP(s)                           - Copy.
        function obj = SP(varargin)
            obj = obj@Spectrum(varargin{:});
        end
    end

    methods (Static)                                                        % Static methods.
        %%% Transformation.
        % SP X = FourierTransform().
        function X = FourierTransform(x, mLR, mFs, mUnit)
            X = SP(mLR, 0, mFs, mUnit);
            w = X.getDomain;
            t = x.getDomain;
            xt = x.value;
            dt = x.unit / x.fs;
            for I = 1 : length(X.value)
                X.value(I) = Math.TrapezoidalIntegral(xt .* exp(-1j .* w(I) .* t), dt);
            end
        end
        % InverseFourierTransform().
        function x = InverseFourierTransform(X, mLR, mFs, mUnit)
            x = SP(mLR, 0, mFs, mUnit);
            t = x.getDomain;
            w = X.getDomain;
            Xw = X.value;
            dw = X.unit / X.fs;
            for I = 1 : length(x.value)
                x.value(I) = 1 / (2 * pi) * Math.TrapezoidalIntegral(Xw .* exp(1j .* t(I) .* w), dw);
            end
        end
    end
end

% Spectrum Class
% By Gralerfics
classdef Spectrum
    properties                                                              % Properties.
        l       % Left border.
        r       % Right border. [l, r)
        unit    % the length of "1".
        fs      % Sampling frequency, = 1 indicates discrete-time. dx = unit / fs.
        value   % Signal value, length = (r - l) * fs.
    end
    
    methods (Access = private)                                              % Inline methods.
        % Fs equality checker.
        function flag = checkFs(A, B)
            if ~isa(A, "Spectrum") || ~isa(B, "Spectrum")
                error("'A' and 'B' should be spectrums(Spectrum).");
            end
            flag = (A.fs == B.fs);
        end

        % Unit length equality checker.
        function flag = checkUnit(A, B)
            if ~isa(A, "Spectrum") || ~isa(B, "Spectrum")
                error("'A' and 'B' should be spectrums(Spectrum).");
            end
            flag = (A.unit == B.unit);
        end

        % Elementary operation, func = @(a, b) a ? b.
        function y = elementaryOperation(A, B, func)
            if ~checkFs(A, B)
                error("'A' and 'B' should have the same sampling frequency(fs).");
            end
            if ~checkUnit(A, B)
                error("'A' and 'B' should have the unit length(unit).");
            end
            L = min(A.l, B.l);
            R = max(A.r, B.r);
            yValue = func(A.cut({L, R}).value, B.cut({L, R}).value);
            y = Spectrum({L, R}, yValue, A.fs, A.unit);
        end

        % Scalar convertor. mA and mB will not be both doubles.
        function [A, B] = convertScalar(mA, mB)
            if isa(mA, "double")
                A = Spectrum({mB.l, mB.r}, mA, mB.fs, mB.unit);
            else
                A = mA;
            end
            if isa(mB, "double")
                B = Spectrum({mA.l, mA.r}, mB, mA.fs, mA.unit);
            else
                B = mB;
            end
        end
    end

    methods                                                                 % Member methods.
        %%% Construction methods.
            % Spectrum({l, r}, value, [fs], [unit]) - All parameters.
                % value can be a scalar.
                % [fs = 1], [unit = 1].
            % Spectrum({l}, value, [fs], [unit])    - Automatically generate r.
            % Spectrum(value, [fs], [unit])         - Default l = 0.
            % Spectrum(s)                           - Copy.
        function obj = Spectrum(varargin)
            % Construct parameter list
            if isa(varargin{1}, "double")
                args = [{{0}}, varargin];
            elseif isa(varargin{1}, "cell")
                args = varargin;
            elseif isa(varargin{1}, "Spectrum")
                s = varargin{1};
                args = {{s.l, s.r}, s.value, s.fs, s.unit};
            end
            % Optional fs and unit
            if length(args) >= 3; obj.fs = args{3}; else; obj.fs = 1; end
            if length(args) >= 4; obj.unit = args{4}; else; obj.unit = 1; end
            % Construct value, l and r
            if length(args{2}) == 1 && length(args{1}) == 2
                obj.value = args{2} * ones(1, (args{1}{2} - args{1}{1}) * obj.fs);
            else
                obj.value = args{2};
            end
            obj.l = args{1}{1};
            if length(args{1}) == 2
                obj.r = args{1}{2};
                if obj.r < obj.l
                    error("'l' should be smaller than 'r'.");
                end
            else
                obj.r = obj.l + length(obj.value) / obj.fs;
            end
            if length(obj.value) ~= (obj.r - obj.l) * obj.fs
                error("Wrong length of 'value'");
            end
        end

        %%% Getters and setters.
        function obj = set.l(obj, mL)
            if fix(mL) == mL
                obj.l = mL;
            else
                error("'l' should be a integer.");
            end
        end
        function obj = set.r(obj, mR)
            if fix(mR) == mR
                obj.r = mR;
            else
                error("'r' should be a integer.");
            end
        end
        function obj = set.fs(obj, mFs)
            if fix(mFs) == mFs && mFs > 0
                obj.fs = mFs;
            else
                error("'fs' should be a positive integer.");
            end
        end
        function obj = set.unit(obj, mUnit)
            if mUnit > 0
                obj.unit = mUnit;
            else
                error("'unit' should be a positive number.");
            end
        end

        %%% Domain operations.
        % getDomain() - return a row vector containing the domain index.
        %               Remember to * unit.
        function y = getDomain(obj)
            y = linspace(obj.l * obj.unit, obj.r * obj.unit, (obj.r - obj.l) * obj.fs + 1);
            y = y(1 : end - 1);
        end
        % cut({l, r}) - [l, r), the part out of range will be filled
        %               with 0.
        function y = cut(obj, mLR)
            if length(mLR) ~= 2 || ~isa(mLR, "cell")
                error("'mLR' should be a cell array contains l and r.");
            end
            lo = (mLR{1} - obj.l) * obj.fs + 1;
            ro = (mLR{2} - obj.l) * obj.fs;
            n = length(obj.value);
            yValue = obj.value(max(1, lo) : min(n, ro));
            yValue = [zeros(1, max(0, 1 - lo)), yValue, zeros(1, max(0, ro - n))];
            y = Spectrum({mLR{1}, mLR{2}}, yValue, obj.fs, obj.unit);
        end
        % reverse() - x(t) -> x(-t).
        function y = reverse(obj)
            y = Spectrum({-obj.r, -obj.l}, obj.value(end : -1 : 1), obj.fs, obj.unit);
        end
        % shift(t0) - x(t) -> x(t - t0). t0 is a integer.
        function y = shift(obj, mT)
            if fix(mT) ~= mT
                error("'t0' should be a integer.");
            end
            y = Spectrum({obj.l - mT, obj.r - mT}, obj.value, obj.fs, obj.unit);
        end
        % toPeriod() - normalize the signal into a period beginning at 0.
        function y = toPeriod(obj)
            y = obj.getPeriod(0);
        end
        % getPeriod(t) - get the period beginning at t.
        function y = getPeriod(obj, mT)
            y = Spectrum({mT}, circshift(obj.value, (obj.l - mT) * obj.fs), obj.fs, obj.unit);
        end
        % lerpShift(t0) - t0 is real.
        function y = lerpShift(obj, mT)
            % TODO
        end
        % lerpFs(newFs) - change sampling frequency with interpolation.
        function y = lerpFs(obj, mFs)
            % TODO
        end

        %%% Operations.
        function y = plus(mA, mB)                                           % +  : Addition
            [A, B] = convertScalar(mA, mB);
            y = elementaryOperation(A, B, @(a, b) a + b);
        end
        function y = minus(mA, mB)                                          % -  : Subtraction
            [A, B] = convertScalar(mA, mB);
            y = elementaryOperation(A, B, @(a, b) a - b);
        end
        function y = uminus(mA)                                             % -  : Opposite
            y = mA;
            y.value = -y.value;
        end
        function y = times(mA, mB)                                          % .* : Multiplication
            [A, B] = convertScalar(mA, mB);
            y = elementaryOperation(A, B, @(a, b) a .* b);
        end
        function y = mtimes(mA, mB)                                         % *  : Convolution
            if isa(mA, "double") || isa(mB, "double")
                y = mA .* mB;
            else
                y = Spectrum.Convolution(mA, mB);
            end
        end
        function y = rdivide(mA, mB)                                        % ./ : Right division
            [A, B] = convertScalar(mA, mB);
            y = elementaryOperation(A, B, @(a, b) a ./ b);
        end
        function y = power(mA, mB)                                          % .^ : Power
            [A, B] = convertScalar(mA, mB);
            y = elementaryOperation(A, B, @(a, b) a .^ b);
        end
        function y = mpower(mA, mB)                                         % ^  : Multi-convolution
            if ~isa(mB, "double") || fix(mB) ~= mB || B < 1
                error("'B' should be a positive integer.");
            end
            y = mA;
            for I = 2 : mB
                y = y * mA;
            end
        end
        function y = uniFunc(obj, name)                                     % func(s)
            y = obj;
            y.value = eval(name + "(y.value)");
        end
        function y = diaFunc(obj, name, arg)                                % func(s, arg0)
            y = obj;
            y.value = eval(name + "(y.value, arg)");
        end
    end

    methods (Static)                                                        % Static methods.
        %%% Specific signals.
        % Identity({l, r}, [fs], [unit]) - y = t, y = n.
        function y = Identity(varargin)
            if nargin >= 2; yFs = varargin{2}; else; yFs = 1; end
            if nargin >= 3; yUnit = varargin{3}; else; yUnit = 1; end
            if length(varargin{1}) ~= 2
                error("'mLR' should be a cell array contains l and r.");
            end
            mLR = varargin{1};
            y = Spectrum(mLR, 0, yFs, yUnit);
            y.value = y.getDomain;
        end
        % Step({l, r}, [fs], [unit]) - y = u(t), y = u[n].
        function y = Step(varargin)
            if nargin >= 2; yFs = varargin{2}; else; yFs = 1; end
            if nargin >= 3; yUnit = varargin{3}; else; yUnit = 1; end
            if length(varargin{1}) ~= 2
                error("'mLR' should be a cell array contains l and r.");
            end
            mLR = varargin{1};
            y = Spectrum(mLR, 1, yFs, yUnit).cut({min(0, mLR{2}), mLR{2}}).cut(mLR);
        end
        % Impulse({l, r}, [fs], [unit]) - y = δ(t), y = δ[n].
        function y = Impulse(varargin)
            if nargin >= 2; yFs = varargin{2}; else; yFs = 1; end
            if nargin >= 3; yUnit = varargin{3}; else; yUnit = 1; end
            if length(varargin{1}) ~= 2
                error("'mLR' should be a cell array contains l and r.");
            end
            mLR = varargin{1};
            y = Spectrum(mLR, 0, yFs, yUnit);
            p = 1 - mLR{1} * yFs;
            if p >= 1 && p <= length(y.value)
                y.value(p) = yFs / yUnit;
            end
        end

        %%% Operations.
        % Convolution(mA, mB) - mA * mB. Remenber ./ fs.
        function y = Convolution(mA, mB)
            if ~checkFs(mA, mB)
                error("'A' and 'B' should have the same sampling frequency(fs).");
            end
            if ~checkUnit(mA, mB)
                error("'A' and 'B' should have the unit length(unit).");
            end
            y = Spectrum({mA.l + mB.l, mA.r + mB.r}, [conv(mA.value, mB.value), 0] ./ mA.fs, mA.fs, mA.unit);
        end
    end
end

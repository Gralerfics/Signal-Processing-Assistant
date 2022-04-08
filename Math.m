% By Gralerfics
classdef Math
    methods (Static)
        %%% Integral
        % Simpson Method
        function s = SimpsonIntegral()

        end
        % Trapezoidal Method, f = [...], dx = Δx.
        function s = TrapezoidalIntegral(f, dx)
            if length(f) < 2 || class(f) ~= "double" || dx < 0 || class(dx) ~= "double"
                error("Wrong parameters.");
            end
            s = dx / 2 * (2 * sum(f) - f(1) - f(end));
        end
    end
end
